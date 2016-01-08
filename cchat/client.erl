-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    if
        St#client_st.connected_server == _Server -> 
            {{error, user_already_connected, "User is already connected to the server."}, St};
        true -> 
            case catch(request(list_to_atom(_Server), {connect, self(), St#client_st.nick})) of
                {'EXIT', {error, nick_taken, Msg}} ->
                    {{error, user_already_connected, Msg}, St};
                {'EXIT', Reason} -> % if the server process cannot be reached
                    {{error, server_not_reached, Reason}, St};
                _Result -> 
                    NewState = St#client_st{connected_server=_Server}, 
                    {ok, NewState}
            end
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    if
        St#client_st.connected_server == no_server_connected -> 
            {{error, user_not_connected, "User is not connected to any server."}, St};
        length(St#client_st.connected_channels) /= 0 -> 
            {{error, leave_channels_first, "User has not left all channels."}, St};
        true -> 
            case catch(request(list_to_atom(St#client_st.connected_server), {disconnect, self(), St#client_st.nick})) of
                {'EXIT', Reason} -> % if the server process cannot be reached
                    {{error, server_not_reached, Reason}, St};
                _Result -> 
                    NewState = St#client_st{connected_server=no_server_connected}, 
                    {ok, NewState}
            end
    end;
    

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->              
	IsConnectedToChannel = lists:member(_Channel, St#client_st.connected_channels), 	
	if
		IsConnectedToChannel == false ->
			request(list_to_atom(St#client_st.connected_server), {join, _Channel, self()}),
			NewChannelList = St#client_st.connected_channels ++ [_Channel],
			NewState = St#client_st{connected_channels=NewChannelList},
			{ok, NewState} ;
		true ->
			{{error, user_already_joined, "User has joined the channel already."}, St}
	end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
	IsConnectedToChannel = lists:member(_Channel, St#client_st.connected_channels), 	
	if
		IsConnectedToChannel == false ->
			{{error, user_not_joined, "User is not connected to that channel."}, St};
        true -> 
			NewChannelList = lists:delete(_Channel, St#client_st.connected_channels),
			NewState = St#client_st{connected_channels = NewChannelList},
			request(list_to_atom(_Channel),{disconnect, self()}),
			{ok, NewState}
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    IsConnectedToChannel = lists:member(_Channel, St#client_st.connected_channels),
    if
		IsConnectedToChannel == false ->
			{{error, user_not_joined, "Tried to write to channel not part of."}, St};
        true -> 
			request(list_to_atom(_Channel), {msg_from_client, self(), St#client_st.nick, _Msg}),
			{ok, St}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#client_st.nick, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
  UserConnected=(St#client_st.connected_server == no_server_connected),
  if
      UserConnected == true ->
             NewState = St#client_st{nick = _Nick}, 
             {ok, NewState};
      false ->
           { {error, user_already_connected,"Disconnect first, to change nickname!"}, St}    
  end;
    
%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #client_st { gui = GUIName }, _MsgFromClient) ->
    {Channel,Name, Msg} = _MsgFromClient,
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


request(_Server, Msg) ->
    genserver:request(_Server, Msg).

initial_state(Nick, GUIName) ->
    #client_st { nick = Nick, connected_server = no_server_connected, gui = GUIName, connected_channels = [] }.

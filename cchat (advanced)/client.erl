-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").


%% ---------------------------------------------------------------------------
% works for both distributed and non distributed server connection
connect(St, _Server) ->
    if
        St#client_st.connected_server == _Server -> 
            {{error, user_already_connected, "User is already connected to the server."}, St};
        true -> 
            case catch(request(distributedPrepareServerAtom(_Server), {connect, self(), St#client_st.nick})) of
                {'EXIT', {error, nick_taken, Msg}} ->
                    {{error, user_already_connected, Msg}, St};
                {'EXIT', Reason} -> % if the server process cannot be reached
                    {{error, server_not_reached, Reason}, St};
                _Result -> 
                    NewState = St#client_st{connected_server=_Server}, 
                    {ok, NewState}
            end
    end.

%% loop handles each kind of request from GUI

%% distributed connection to server
loop(St, {connect, {_Server, Machine}}) ->         
    connect(St, {_Server,Machine});
    % {{error, not_implemented, "Not implemented"}, St} ;

%% connection to server
loop(St, {connect, _Server}) ->   
    connect(St,_Server);
    % {{error, not_implemented, "Not implemented"}, St} ;


%% Disconnect from server
loop(St, disconnect) ->
    if
        St#client_st.connected_server == no_server_connected -> 
            {{error, user_not_connected, "User is not connected to any server."}, St};
        length(St#client_st.connected_channels) /= 0 -> 
            {{error, leave_channels_first, "User has not left all channels."}, St};
        true -> 
            case catch(request(distributedPrepareServerAtom(St#client_st.connected_server), {disconnect, self(), St#client_st.nick})) of
                {'EXIT', Reason} -> % if the server process cannot be reached
                    {{error, server_not_reached, Reason}, St};
                _Result -> 
                    NewState = St#client_st{connected_server=no_server_connected}, 
                    {ok, NewState}
            end
    end;
    % {{error, not_implemented, "Not implemented"}, St} ;

% Join channel
loop(St, {join, _Channel}) ->
    IsConnectedToChannel = lists:member(_Channel, St#client_st.connected_channels),     
    if
        IsConnectedToChannel == false ->
            request(distributedPrepareServerAtom(St#client_st.connected_server), {join, _Channel, self()}),
            NewChannelList = St#client_st.connected_channels ++ [_Channel],
            NewState = St#client_st{connected_channels=NewChannelList},
            {ok, NewState} ;
        true ->
            {{error, user_already_joined, "User has joined the channel already."}, St}
    end;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
loop(St, {leave, _Channel}) ->
    IsConnectedToChannel = lists:member(_Channel, St#client_st.connected_channels),     
    if
        IsConnectedToChannel == false ->
            {{error, user_not_joined, "User is not connected to that channel."}, St};
        true -> 
            NewChannelList = lists:delete(_Channel, St#client_st.connected_channels),
            NewState = St#client_st{connected_channels = NewChannelList},
            request(distributedPrepareChannelAtom(St#client_st.connected_server, _Channel),{disconnect, self()}),
            {ok, NewState}
    end;
    % {{error, not_implemented, "Not implemented"}, St} ;

% Sending messages
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    % Check if user is connected to a channel he/she is sending from
    IsConnectedToChannel = lists:member(_Channel, St#client_st.connected_channels),
    if
        IsConnectedToChannel == false ->
            {{error, user_not_joined, "Tried to write to channel not part of."}, St};
        true -> 
            request(distributedPrepareChannelAtom(St#client_st.connected_server, _Channel), {msg_from_client, self(), St#client_st.nick, _Msg}),
            {ok, St}
    end;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St, whoami) ->
    % return nickname from the client_st record of the client with the current process id
    {St#client_st.nick, St};
    % {{error, not_implemented, "Not implemented"}, St} ;


%% Change nick
loop(St, {nick, _Nick}) ->
    % return nickname from the client_st record of the client with the current process id
    UserConnected=(St#client_st.connected_server == no_server_connected),
      if
          UserConnected =/= false ->
                 NewState = St#client_st{nick = _Nick}, 
                 {ok, NewState};
          true ->
               { {error, user_already_connected,"Disconnect first, to change nickname!"}, St}    
      end;
    % {{error, not_implemented, "Not implemented"}, St} ;


%% Incoming message
loop(St = #client_st { gui = GUIName }, _MsgFromClient) ->
    {Channel, Name, Msg} = _MsgFromClient,
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


distributedPrepareServerAtom({Server, Machine}) ->
    {list_to_atom(Server),list_to_atom(Machine)};

distributedPrepareServerAtom(Server) ->
    list_to_atom(Server).

distributedPrepareChannelAtom({_Server, Machine}, Channel) ->
    {list_to_atom(Channel), list_to_atom(Machine)};

distributedPrepareChannelAtom(_Server, _Channel) ->
    list_to_atom(_Channel).

%% makes standard request call that is to be sent to the server
request(_Server, _Msg) ->
    genserver:request(_Server, _Msg).


%% Produce initial state for the client
initial_state(Nick, GUIName) ->
    #client_st { nick = Nick, connected_server = no_server_connected, gui = GUIName, connected_channels = [] }.



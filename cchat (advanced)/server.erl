-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

%% ---------------------------------------------------------------------------

% Connecting client to the server.
loop(St, {connect, Pid, Nick}) ->
    NickTaken = lists:member(Nick, St#server_st.nicknames),
    if
        NickTaken == false ->
            NewClientList = St#server_st.clients ++ [Pid],    
            NewNicknameList = St#server_st.nicknames ++ [Nick],
            NewState = St#server_st{clients=NewClientList, nicknames=NewNicknameList},
            {ok, NewState};
        true -> 
            {'EXIT', {error, nick_taken, "Nick already taken on server"}}
    end;
    
% Disconnect client from the server
loop(St, {disconnect, Pid, Nick}) ->
    NewClientList = lists:delete(Pid, St#server_st.clients),
    NewNicknameList = lists:delete(Nick, St#server_st.nicknames),
    NewState = St#server_st{clients=NewClientList, nicknames=NewNicknameList},
    {ok, NewState};

% Joining a client to a Channel
loop(St, {join, _Channel, Pid}) ->
	ChannelExists = lists:member(_Channel, St#server_st.channels),
    if  
        ChannelExists == false ->   
            NewChannelList = St#server_st.channels ++ [_Channel],
            NewState = St#server_st{channels=NewChannelList},
            start_channel_process(_Channel),
            genserver:request(list_to_atom(_Channel), {connect, Pid}),
            {ok, NewState};
        true ->         
            genserver:request(list_to_atom(_Channel), {connect, Pid}),
            {ok, St}    
    end.


start_channel_process(_Channel) ->
    genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2).


% Produce initial state for the server
initial_state(_Server) ->
    #server_st{clients=[], channels=[], nicknames=[]}.

%% ---------------------------------------------------------------------------

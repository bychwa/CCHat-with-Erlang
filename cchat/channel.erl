-module(channel).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, Pid}) ->
    NewClientList = St#channel_st.clients ++ [Pid],
    NewState = St#channel_st{clients=NewClientList},
    {ok, NewState};

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%    
loop(St, {disconnect, Pid}) ->
    NewClientList = lists:delete(Pid, St#channel_st.clients),
    NewState = St#channel_st{clients=NewClientList},
    {ok, NewState};

%%%%%%%%%%%%%%%
%%%% Send message
%%%%%%%%%%%%%%%
loop(St, {msg_from_client, Pid, Name, _Msg}) ->
	spawn(fun() -> new_process(St, Pid, Name, _Msg) end),
	{ok, St}.

new_process(St, Pid, Name, _Msg) ->
	SendToList = lists:delete(Pid, St#channel_st.clients),
		send_msg_to_client(SendToList, St#channel_st.name, Name, _Msg).

send_msg_to_client([SendTo|ClientList], Channel, Name, _Msg) ->
	spawn(fun() -> genserver:request(SendTo, {Channel, Name, _Msg})end),
	send_msg_to_client(ClientList, Channel, Name, _Msg);

send_msg_to_client([], _Channel, _Name, _Msg) ->
	do_nothing.

initial_state(_Channel) ->
    #channel_st{clients=[], name=_Channel}.
    


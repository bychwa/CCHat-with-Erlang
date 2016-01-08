
% This record defines the structure of the client process:
% nick: Holds the nickname of the client.
% gui: Holds the name (or Pid) of the GUI process.
% connected_server: Holds the name (or PID) of the server the client is connected to.
% connected_channels: Holds the name (or PID) of the channels the client is connected to.
-record(client_st, {nick, gui, connected_server, connected_channels}).


% This record defines the structure of the server process:
% clients: Holds the name (or Pid) of the client processes connected to the server.
% channels: Holds the name (or PID) of the channels the server is connected to.
% nicknames: Holds the nicknames of all the clients connected to the server.
-record(server_st, {clients, channels, nicknames}).


% This record defines the structure of the channel process:
% clients: Holds the name (or Pid) of the client processes connected to the channel.
% name: Holds the name of the channel.
-record(channel_st, {clients, name}).
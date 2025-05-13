%% This module echoes client events back,
%% including creating new streams.

-module(wt_echo_h).
%% @todo -behavior(cowboy_webtransport).

-export([init/2]).
-export([webtransport_handle/2]).

init(Req, _) ->
	{cowboy_webtransport, Req, undefined}.

%% @todo WT handle {stream_open,4,bidi}
%% @todo WT handle {stream_data,4,nofin,<<>>} %% skip?

webtransport_handle(Event = {stream_data, StreamID, IsFin, Data}, HandlerState) ->
	ct:pal("WT handle ~p~n", [Event]),
	{[{send, StreamID, Data}], HandlerState};
webtransport_handle(Event, HandlerState) ->
	ct:pal("WT handle ~p~n", [Event]),
	{[], HandlerState}.

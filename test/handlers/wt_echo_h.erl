%% This module echoes client events back,
%% including creating new streams.

-module(wt_echo_h).
%% @todo -behavior(cowboy_webtransport).

-export([init/2]).
-export([webtransport_handle/2]).
-export([webtransport_info/2]).

init(Req, _) ->
	{cowboy_webtransport, Req, #{}}.

%% @todo WT handle {stream_open,4,bidi}

webtransport_handle(Event = {stream_open, StreamID, bidi}, Streams) ->
	ct:pal("WT handle ~p~n", [Event]),
	{[], Streams#{StreamID => bidi}};
webtransport_handle(Event = {stream_open, StreamID, unidi}, Streams) ->
	ct:pal("WT handle ~p~n", [Event]),
	OpenStreamRef = make_ref(),
	{[{open_stream, OpenStreamRef, unidi, <<>>}], Streams#{
		StreamID => {unidi_remote, OpenStreamRef},
		OpenStreamRef => {unidi_local, StreamID}}};
webtransport_handle(Event = {opened_stream_id, OpenStreamRef, OpenStreamID}, Streams) ->
	ct:pal("WT handle ~p~n", [Event]),
	#{OpenStreamRef := {unidi_local, RemoteStreamID}} = Streams,
	#{RemoteStreamID := {unidi_remote, OpenStreamRef}} = Streams,
	{[], maps:remove(OpenStreamRef, Streams#{
		RemoteStreamID => {unidi_remote, OpenStreamID},
		OpenStreamID => {unidi_local, RemoteStreamID}
	})};
webtransport_handle(Event = {stream_data, StreamID, IsFin, Data}, Streams) ->
	ct:pal("WT handle ~p~n", [Event]),
	case Streams of
		#{StreamID := bidi} ->
			{[{send, StreamID, IsFin, Data}], Streams};
		#{StreamID := {unidi_remote, Ref}} when is_reference(Ref) ->
			%% The stream isn't ready. We try again later.
			erlang:send_after(100, self(), {try_again, Event}),
			{[], Streams};
		#{StreamID := {unidi_remote, LocalStreamID}} ->
			{[{send, LocalStreamID, IsFin, Data}], Streams}
	end;
webtransport_handle(Event, Streams) ->
	ct:pal("WT handle ~p~n", [Event]),
	{[], Streams}.

webtransport_info({try_again, Event}, Streams) ->
	ct:pal("try_again ~p", [Event]),
	webtransport_handle(Event, Streams).

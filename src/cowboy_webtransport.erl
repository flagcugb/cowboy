%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @todo To enable WebTransport the following options need to be set:
%%
%% QUIC:
%%  - max_datagram_frame_size > 0
%%
%% HTTP/3:
%%  - SETTINGS_H3_DATAGRAM = 1
%%  - SETTINGS_ENABLE_CONNECT_PROTOCOL = 1
%%  - SETTINGS_WEBTRANSPORT_MAX_SESSIONS >= 1

%% Cowboy supports versions 7 through 12 of the WebTransport drafts.
-module(cowboy_webtransport).

-export([upgrade/4]).
-export([upgrade/5]).

-type opts() :: #{
	%% @todo
}.
-export_type([opts/0]).

-record(state, {
	parent :: pid(),
	opts = #{} :: opts(),
	handler :: module(),
	hibernate = false :: boolean(),
	req = #{} :: map()
}).

%% This function mirrors a similar function for Websocket.

-spec is_upgrade_request(cowboy_req:req()) -> boolean().
is_upgrade_request(#{version := Version, method := <<"CONNECT">>, protocol := Protocol})
		when Version =:= 'HTTP/3' ->
	%% @todo scheme MUST BE "https"
	<<"webtransport">> =:= cowboy_bstr:to_lower(Protocol);

is_upgrade_request(_) ->
	false.

%% Stream process.

-spec upgrade(Req, Env, module(), any())
	-> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().

upgrade(Req, Env, Handler, HandlerState) ->
	upgrade(Req, Env, Handler, HandlerState, #{}).

-spec upgrade(Req, Env, module(), any(), opts())
	-> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().

%% @todo Immediately crash if a response has already been sent.
upgrade(Req=#{version := 'HTTP/3', pid := Pid, streamid := StreamID}, Env, Handler, HandlerState, Opts) ->
	FilteredReq = case maps:get(req_filter, Opts, undefined) of
		undefined -> maps:with([method, version, scheme, host, port, path, qs, peer], Req);
		FilterFun -> FilterFun(Req)
	end,
	%% @todo add parent, ref, streamid here directly
	State = #state{parent=Pid, opts=Opts, handler=Handler, req=FilteredReq},

	%% @todo Must check is_upgrade_request (rename, not an upgrade)
	%% and also ensure that all the relevant settings are enabled (quic and h3)

	%% @todo A protocol may be negotiated via
	%% - WT-Available-Protocols
	%% - WT-Protocol
	%% Negotiation is done by the handler in init like Websocket.
	%% Parsing and building of the headers must be added to Cowlib though.

	%% Considering we must ensure the relevant settings are enabled,
	%% either we check them BEFORE, or we check them when the handler
	%% is OK to initiate a webtransport session. Probably need to
	%% check them BEFORE as we need to become (takeover) the webtransport process
	%% after we are done with the upgrade. -> we check them in cow_http3_machine OK

	%% After the upgrade we become the process that will receive all data
	%% relevant to this webtransport session. However the data will not
	%% go through stream handlers / middlewares anymore, it will be
	%% a straight cowboy_http3 -> this pid.

	case is_upgrade_request(Req) of
		true ->
			Headers = cowboy_req:response_headers(#{}, Req),
			Pid ! {{Pid, StreamID}, {switch_protocol, Headers, ?MODULE,
				#{session_pid => self()}}},
			webtransport_init(State, HandlerState);
		%% Use 501 Not Implemented to mirror the recommendation in
		%% by RFC9220 3 (WebSockets Upgrade over HTTP/3).
		false ->
			{ok, cowboy_req:reply(501, Req), Env}
	end.

webtransport_init(State=#state{handler=Handler}, HandlerState) ->
	case erlang:function_exported(Handler, webtransport_init, 1) of
		true -> handler_call(State, HandlerState, webtransport_init, undefined);
		false -> before_loop(State, HandlerState)
	end.

before_loop(State=#state{hibernate=true}, HandlerState) ->
	proc_lib:hibernate(?MODULE, loop, [State#state{hibernate=false}, HandlerState]);
before_loop(State, HandlerState) ->
	loop(State, HandlerState).

-spec loop(#state{}, any()) -> no_return().

loop(State=#state{parent=Parent%, timeout_ref=TRef
		}, HandlerState) ->
	receive
		%% @todo Parent to filter messages? Nothing?
		%% @todo Can there be groups of events?
		{'$webtransport_event', Event} ->
			handler_call(State, HandlerState, webtransport_handle, Event);
		%% Timeouts.
%% @todo idle_timeout
%		{timeout, TRef, ?MODULE} ->
%			tick_idle_timeout(State, HandlerState, ParseState);
%		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
%			before_loop(State, HandlerState, ParseState);
		%% System messages.
		{'EXIT', Parent, Reason} ->
			%% @todo We should exit gracefully.
			exit(Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{State, HandlerState});
		%% Calls from supervisor module.
		{'$gen_call', From, Call} ->
			cowboy_children:handle_supervisor_call(Call, From, [], ?MODULE),
			before_loop(State, HandlerState);
		Message ->
			handler_call(State, HandlerState, websocket_info, Message)
	end.

handler_call(State=#state{handler=Handler}, HandlerState, Callback, Message) ->
	try case Callback of
		websocket_init -> Handler:websocket_init(HandlerState);
		_ -> Handler:Callback(Message, HandlerState)
	end of
		{Commands, HandlerState2} when is_list(Commands) ->
			handler_call_result(State, HandlerState2, Commands);
		{Commands, HandlerState2, hibernate} when is_list(Commands) ->
			handler_call_result(State#state{hibernate=true}, HandlerState2, Commands)
	catch Class:Reason:Stacktrace ->
%% @todo
%		websocket_send_close(State, {crash, Class, Reason}),
%		handler_terminate(State, HandlerState, {crash, Class, Reason}),
		erlang:raise(Class, Reason, Stacktrace)
	end.

handler_call_result(State0, HandlerState, Commands) ->
	case commands(Commands, State0, []) of
		{ok, State} ->
			before_loop(State, HandlerState);
		{stop, State} ->
			terminate(State, HandlerState, stop);
		{Error = {error, _}, State} ->
			terminate(State, HandlerState, Error)
	end.

%% We accumulate the commands that must be sent to the connection process
%% because we want to send everything into one message. Other commands are
%% processed immediately.

commands([], State, []) ->
	{ok, State};
commands([], State=#state{parent=Pid}, Commands) ->
	Pid ! {'$webtransport_commands', lists:reverse(Commands)},
	{ok, State};
%% {open_stream, OpenStreamRef, StreamType, InitialData}.
commands([Command={open_stream, _, _, _}|Tail], State, Acc) ->
	commands(Tail, State, [Command|Acc]);
%% {close_stream, StreamID, Code}.
commands([Command={close_stream, _, _}|Tail], State, Acc) ->
	commands(Tail, State, [Command|Acc]);
%% {send, StreamID | datagram, Data}.
commands([Command={send, _, _}|Tail], State, Acc) ->
	commands(Tail, State, [Command|Acc]).
%% @todo send with IsFin
%% @todo stop, {error, Reason} probably. What to do about sending when asked to stop?
%% @todo set_options (to increase number of streams? data amounts? or a flow command?)
%% @todo shutdown_reason if useful.

terminate(State, HandlerState, Error) ->
	error({todo, State, HandlerState, Error}).







%% WebTransport functions:
%%
%% webtransport_init(HandlerState)
%% webtransport_handle({opened_stream_id, OpenStreamRef, StreamID}, HandlerState)
%% webtransport_handle({stream_open, StreamID, unidi | bidi}, HandlerState)
%% webtransport_handle({stream_data, StreamID, IsFin, Data}, HandlerState)
%% webtransport_handle({stream_reset_at, StreamID, Code, FinalSize}, HandlerState)
%% webtransport_handle({stream_stop_sending, StreamID, Code}, HandlerState)
%% webtransport_handle({datagram, Data}, HandlerState)
%% webtransport_handle(goaway, HandlerState)
%% webtransport_info(Message, HandlerState)

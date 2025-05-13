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

-module(draft_h3_webtransport_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, enabled}].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{enabled, [], Tests}]. %% @todo Enable parallel when all is better.

init_per_group(Name = enabled, Config) ->
	cowboy_test:init_http3(Name, #{
		enable_connect_protocol => true,
		h3_datagram => true,
		enable_webtransport => true, %% For compatibility with draft-02.
		webtransport_max_sessions => 10,
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

init_routes(_) -> [
	{"localhost", [
		{"/wt", wt_echo_h, []}
	]}
].

%% Temporary.

%% To start Chromium the command line is roughly:
%% chromium --ignore-certificate-errors-spki-list=LeLykt63i2FRAm+XO91yBoSjKfrXnAFygqe5xt0zgDA= --ignore-certificate-errors --user-data-dir=/tmp/chromium-wt --allow-insecure-localhost --webtransport-developer-mode --enable-quic https://googlechrome.github.io/samples/webtransport/client.html
%%
%% To find the SPKI the command is roughly:
%% openssl x509 -in ~/ninenines/cowboy/test/rfc9114_SUITE_data/server.pem -pubkey -noout | \ 
%%  openssl pkey -pubin -outform der | \
%%  openssl dgst -sha256 -binary | \
%%  openssl enc -base64

run(_Config) ->
	timer:sleep(infinity).

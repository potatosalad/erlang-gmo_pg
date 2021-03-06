%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2018, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  22 Nov 2018 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(gmo_pg_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
	URIConfig = [
		{{uri, <<"ftp">>}, 21},
		{{uri, <<"sftp">>}, 22},
		{{uri, <<"tftp">>}, 69},
		{{uri, <<"http">>}, 80},
		{{uri, <<"https">>}, 443},
		{{uri, <<"ldap">>}, 389}
	],
	Config = URIConfig,
	gmo_pg_config = gmo_pg_config:new(Config),
	ChildSpecs = [
		?CHILD(gmo_pg_config, worker)
	],
	Restart = {one_for_one, 1, 5},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

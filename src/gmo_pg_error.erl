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
-module(gmo_pg_error).

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	code := binary(),
	info := binary(),
	message := nil | binary(),
	data := map()
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([new/2]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		code => nil,
		info => nil,
		message => nil,
		data => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

%%%===================================================================
%%% API functions
%%%===================================================================

new(_Client = #{'__struct__' := gmo_pg_client, locale := Locale}, Data = #{<<"ErrCode">> := Code, <<"ErrInfo">> := Info}) ->
	Message = gmo_pg_const:safe_error_msg(Info, Locale),
	'__struct__'(#{
		code => Code,
		info => Info,
		message => Message,
		data => Data
	}).

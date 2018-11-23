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
-module(gmo_pg_client).

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	shop_id := nil | binary(),
	shop_pass := nil | binary(),
	site_id := nil | binary(),
	site_pass := nil | binary(),
	endpoint := gmo_pg_uri:t(),
	locale := atom(),
	options := list(),
	user_agent := nil | binary()
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([new/1]).
-export([decode_params/1]).
-export([encode_params/1]).
-export([error_msg/2]).
-export([user_agent/0]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		shop_id => nil,
		shop_pass => nil,
		site_id => nil,
		site_pass => nil,
		endpoint => nil,
		locale => en,
		options => [],
		user_agent => user_agent()
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	Client0 = maps:fold(fun maps:update/3, '__struct__'(), Map),
	Client1 = validate_locale(Client0),
	Client2 = validate_endpoint(Client1, Map),
	Client2.

%%%===================================================================
%%% API functions
%%%===================================================================

new(Map) when is_map(Map) ->
	'__struct__'(Map);
new(List) when is_list(List) ->
	new(maps:from_list(List)).

decode_params(Params0) when is_map(Params0) ->
	Params = maps:fold(fun do_decode_param/3, maps:new(), Params0),
	Params.

encode_params(Params0) when is_map(Params0) ->
	Params = maps:fold(fun do_encode_param/3, maps:new(), Params0),
	Params.

error_msg(#{'__struct__' := ?MODULE, locale := Locale}, Code) ->
	gmo_pg_const:error_msg(Code, Locale).

user_agent() ->
	_ = application:load(gmo_pg),
	{ok, Version} = application:get_key(gmo_pg, vsn),
	SystemArchitecture = erlang:system_info(system_architecture),
	OTPRelease = otp_release_version(),
	SystemVersion = erlang:system_info(version),
	[{_, _, OpenSSL} | _] = crypto:info_lib(),
	_ = application:load(hackney),
	{ok, HackneyVersion} = application:get_key(hackney, vsn),
	case maybe_elixir_version() of
		{ok, ElixirVersion} ->
			erlang:iolist_to_binary(io_lib:format(
				"erlang-gmo_pg/~s "
				"(~s; ~s) "
				"elixir/~s erlang/~s erts/~s hackney/~s",
				[Version, SystemArchitecture, OpenSSL, ElixirVersion, OTPRelease, SystemVersion, HackneyVersion]));
		error ->
			erlang:iolist_to_binary(io_lib:format(
				"erlang-gmo_pg/~s "
				"(~s; ~s) "
				"erlang/~s erts/~s hackney/~s",
				[Version, SystemArchitecture, OpenSSL, OTPRelease, SystemVersion, HackneyVersion]))
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
do_decode_param(Key, Value0, Acc) when is_binary(Value0) ->
	Value = iconv:convert(<<"cp932">>, <<"utf-8">>, Value0),
	maps:put(Key, Value, Acc).

%% @private
do_encode_param(Key, Value0, Acc) when is_binary(Value0) ->
	Value = iconv:convert(<<"utf-8">>, <<"cp932">>, Value0),
	maps:put(Key, Value, Acc).

%% @private
maybe_elixir_version() ->
	_ = code:ensure_loaded('Elixir.System'),
	case erlang:function_exported('Elixir.System', version, 0) of
		true ->
			try 'Elixir.System':version() of
				Version when is_binary(Version) ->
					{ok, Version};
				_ ->
					error
			catch
				_:_ ->
					error
			end;
		false ->
			error
	end.

%% @private
otp_release_version() ->
	OTPRelease = erlang:system_info(otp_release),
	try file:read_file(filename:join([code:root_dir(), "releases", OTPRelease, "OTP_VERSION"])) of
		{ok, Version} ->
			VersionBinary = erlang:iolist_to_binary(Version),
			VersionStripped = binary:split(VersionBinary, [<<$\r>>, <<$\n>>], [global, trim_all]),
			erlang:iolist_to_binary(VersionStripped);
		{error, _Reason} ->
			erlang:iolist_to_binary(OTPRelease)
	catch
		_:_ ->
			erlang:iolist_to_binary(OTPRelease)
	end.

%% @private
validate_endpoint(Client = #{endpoint := Endpoint0}, Map) ->
	case Endpoint0 of
		<<_/binary>> ->
			Endpoint = #{'__struct__' := 'Elixir.URI'} = gmo_pg_uri:parse(Endpoint0),
			Client#{endpoint := Endpoint};
		nil ->
			erlang:error({missing_required_params, [endpoint], Map})
	end.

%% @private
validate_locale(Client = #{locale := Locale}) ->
	try gmo_pg_const:error_msg(<<"000">>, Locale) of
		<<_/binary>> ->
			Client
	catch
		error:function_clause ->
			erlang:error({locale_not_supported, Locale})
	end.

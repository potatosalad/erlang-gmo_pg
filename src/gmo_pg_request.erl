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
-module(gmo_pg_request).

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	client := gmo_pg_client:t(),
	method := get | post,
	url := binary(),
	headers := [{binary(), binary()}],
	params := map()
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([new/4]).
-export([build_hackney_request/1]).
-export([execute/1]).
-export([execute/2]).
-export([put_header/3]).
-export([put_new_header/3]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		client => nil,
		method => nil,
		url => nil,
		headers => [],
		params => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

%%%===================================================================
%%% API functions
%%%===================================================================

new(Client = #{
	'__struct__' := gmo_pg_client,
	endpoint := Endpoint,
	user_agent := UserAgent
}, Method, Name, Params0)
		when (Method =:= get orelse Method =:= post)
		andalso is_binary(Name)
		andalso is_map(Params0) ->
	URL =
		case Name of
			<<>> ->
				gmo_pg_uri:to_string(Endpoint);
			<<_/binary>> ->
				gmo_pg_uri:to_string(gmo_pg_uri:append_path(Endpoint, Name))
		end,
	Params = gmo_pg_const:cast_input_params(Params0),
	Req0 = '__struct__'(#{
		client => Client,
		method => Method,
		url => URL,
		params => Params
	}),
	Req1 = put_new_header(Req0, <<"user-agent">>, UserAgent),
	Req1.

build_hackney_request(#{
	'__struct__' := ?MODULE,
	method := Method,
	url := URL,
	headers := Headers,
	params := Params0
}) ->
	Params = gmo_pg_client:encode_params(Params0),
	case Method of
		get ->
			NewURL = gmo_pg_uri:to_string(gmo_pg_uri:append_query(URL, Params)),
			{Method, NewURL, Headers, <<>>};
		post ->
			Body = {form, maps:to_list(Params)},
			{Method, URL, Headers, Body}
	end.

execute(Request = #{'__struct__' := ?MODULE, client := #{'__struct__' := gmo_pg_client, options := Options}}) when is_list(Options) ->
	execute(Request, Options).

execute(Request = #{
	'__struct__' := ?MODULE,
	client := Client
}, Opts0) ->
	{Resolve, Opts1} =
		case lists:keytake(resolve, 1, Opts0) of
			{value, {resolve, R}, ROpts} when R =:= false orelse R =:= once orelse R =:= true ->
				{R, ROpts};
			false ->
				{false, Opts0}
		end,
	{Method, URL, Headers, Body} = build_hackney_request(Request),
	Result =
		case hackney:request(Method, URL, Headers, Body, Opts1) of
			{ok, ClientRef} when is_reference(ClientRef) ->
				Response = gmo_pg_response:new(Client, ClientRef),
				{ok, Response};
			{ok, StatusCode, RespHeaders} ->
				Response = gmo_pg_response:new(Client, StatusCode, RespHeaders),
				{ok, Response};
			{ok, StatusCode, RespHeaders, ClientRef} ->
				Response = gmo_pg_response:new(Client, StatusCode, RespHeaders, ClientRef),
				{ok, Response};
			{error, Reason} ->
				{error, {Request, Reason}}
		end,
	case Result of
		{ok, Resp0} ->
			case Resolve of
				false ->
					{ok, Resp0};
				once ->
					gmo_pg_response:resolve_once(Resp0);
				true ->
					gmo_pg_response:resolve(Resp0)
			end;
		Error = {error, _} ->
			Error
	end.

put_header(Req0 = #{'__struct__' := ?MODULE, headers := H0}, Header0, Value)
		when is_binary(Header0) andalso is_binary(Value) ->
	Header = gmo_pg_util:lowercase(Header0),
	H1 = lists:keystore(Header, 1, H0, {Header, Value}),
	Req1 = Req0#{headers := H1},
	Req1.

put_new_header(Req0 = #{'__struct__' := ?MODULE, headers := H0}, Header0, Value)
		when is_binary(Header0) andalso is_binary(Value) ->
	Header = gmo_pg_util:lowercase(Header0),
	case lists:keymember(Header, 1, H0) of
		false ->
			H1 = lists:keystore(Header, 1, H0, {Header, Value}),
			Req1 = Req0#{headers := H1},
			Req1;
		true ->
			Req0
	end.

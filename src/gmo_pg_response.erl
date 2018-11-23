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
-module(gmo_pg_response).

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	client := gmo_pg_client:t(),
	client_ref := nil | reference(),
	status_code := nil | non_neg_integer(),
	headers := nil | list(),
	body := nil | binary(),
	data := nil | map(),
	list := nil | [map()],
	errors := nil | [map()]
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([new/2]).
-export([new/3]).
-export([new/4]).
-export([resolve/1]).
-export([resolve_once/1]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		client => nil,
		client_ref => nil,
		status_code => nil,
		headers => nil,
		body => nil,
		data => nil,
		list => nil,
		errors => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

%%%===================================================================
%%% API functions
%%%===================================================================

new(Client = #{'__struct__' := gmo_pg_client}, ClientRef)
		when is_reference(ClientRef) ->
	'__struct__'(#{
		client => Client,
		client_ref => ClientRef
	}).

new(Client = #{'__struct__' := gmo_pg_client}, StatusCode, Headers)
		when is_integer(StatusCode)
		andalso is_list(Headers) ->
	'__struct__'(#{
		client => Client,
		status_code => StatusCode,
		headers => Headers
	}).

new(Client = #{'__struct__' := gmo_pg_client}, StatusCode, Headers, ClientRef)
		when is_integer(StatusCode)
		andalso is_list(Headers)
		andalso is_reference(ClientRef) ->
	'__struct__'(#{
		client => Client,
		client_ref => ClientRef,
		status_code => StatusCode,
		headers => Headers
	}).

resolve(Resp0 = #{'__struct__' := ?MODULE}) ->
	case resolve_once(Resp0) of
		{ok, Resp0} ->
			{ok, Resp0};
		{ok, Resp1 = #{'__struct__' := ?MODULE}} ->
			resolve(Resp1);
		{error, Reason} ->
			{error, Reason}
	end.

resolve_once(Resp = #{
	'__struct__' := ?MODULE,
	status_code := StatusCode,
	headers := Headers,
	client_ref := ClientRef,
	body := <<_/binary>>
})
		when is_integer(StatusCode)
		andalso is_list(Headers)
		andalso is_reference(ClientRef) ->
	{ok, Resp};
resolve_once(Resp0 = #{
	'__struct__' := ?MODULE,
	status_code := StatusCode,
	headers := Headers,
	client_ref := ClientRef,
	body := nil
})
		when is_integer(StatusCode)
		andalso is_list(Headers)
		andalso is_reference(ClientRef) ->
	case hackney:body(ClientRef) of
		{ok, Body} ->
			handle_body(Resp0, Body);
		{error, Reason} ->
			{error, {Resp0, Reason}}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
handle_body(Resp0 = #{'__struct__' := ?MODULE, body := nil, data := nil, list := nil, errors := nil}, Body) ->
	Resp1 = Resp0#{body := Body},
	case parse_body(Body) of
		{ok, Data} ->
			Resp2 = Resp1#{data := Data},
			Resp3 = maybe_parse_list(Resp2),
			Resp4 = maybe_parse_errors(Resp3),
			{ok, Resp4};
		{error, Reason} ->
			{error, {Resp1, Reason}}
	end.

%% @private
maybe_parse_errors(Resp0 = #{'__struct__' := ?MODULE, client := Client, list := List, errors := nil}) when is_list(List) ->
	Errors = parse_errors(List, Client, []),
	Resp1 = Resp0#{errors := Errors},
	Resp1.

%% @private
maybe_parse_list(Resp0 = #{'__struct__' := ?MODULE, data := Data, list := nil}) when is_map(Data) ->
	List = parse_list(Data),
	Resp1 = Resp0#{list := List},
	Resp1.

%% @private
parse_body(Body) when is_binary(Body) ->
	try gmo_pg_uri:decode_query(Body) of
		Data0 when is_map(Data0) ->
			Data = gmo_pg_client:decode_params(Data0),
			{ok, Data}
	catch
		error:Reason ->
			{error, Reason}
	end.

%% @private
parse_errors([Data = #{<<"ErrCode">> := _, <<"ErrInfo">> := _} | Rest], Client, Acc) ->
	Error = gmo_pg_error:new(Client, Data),
	parse_errors(Rest, Client, [Error | Acc]);
parse_errors([_ | Rest], Client, Acc) ->
	parse_errors(Rest, Client, Acc);
parse_errors([], _Client, Acc) ->
	lists:reverse(Acc).

%% @private
parse_list(Data) when is_map(Data) ->
	case maps:fold(fun parse_list_fold/3, {nil, maps:new()}, Data) of
		{0, _ListData} ->
			[Data];
		{Count, ListData} ->
			parse_list_expand(ListData, 1, Count, []);
		false ->
			[]
	end.

%% @private
parse_list_expand(_ListData, N, C, Acc) when N > C ->
	lists:reverse(Acc);
parse_list_expand(ListData, N, C, Acc) when N =< C ->
	Row = parse_list_expand_once(ListData, N),
	parse_list_expand(ListData, N + 1, C, [Row | Acc]).

%% @private
parse_list_expand_once(ListData, N) when is_map(ListData) andalso is_integer(N) andalso N > 0 ->
	Folder = fun(Key, List, Acc) ->
		maps:put(Key, lists:nth(N, List), Acc)
	end,
	maps:fold(Folder, maps:new(), ListData).

%% @private
parse_list_fold(_Key, _Value, false) ->
	false;
parse_list_fold(Key, Value, {C, Acc}) when C =:= nil orelse is_integer(C) ->
	List = binary:split(Value, <<$|>>, [global]),
	case length(List) of
		ListCount when C =:= nil ->
			{ListCount, maps:put(Key, List, Acc)};
		C ->
			{C, maps:put(Key, List, Acc)};
		_ ->
			false
	end.

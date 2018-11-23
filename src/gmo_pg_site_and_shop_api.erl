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
-module(gmo_pg_site_and_shop_api).

%% Public API
-export([traded_card/2]).
-export([traded_brandtoken/2]).
-export([exec_tran_brandtoken/2]).
%% Request API
-export([request/4]).

%% Macros
-define(valid_client, #{
	'__struct__' := gmo_pg_client,
	shop_id := <<_/binary>>,
	shop_pass := <<_/binary>>,
	site_id := <<_/binary>>,
	site_pass := <<_/binary>>
}).

%%%===================================================================
%%% Public API functions
%%%===================================================================

% # 2.17.2.1.決済後カード登録
% # 指定されたオーダーID の取引に使用したカードを登録します。
% ### @return ###
% # CardSeq
% # CardNo
% # Forward
traded_card(Client = ?valid_client, Params = #{member_id := <<_/binary>>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/TradedCard.idPass">>,
	request(Client, post, Name, Params);
traded_card(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id, order_id], Params}).

% ### @params ###
% # MemberID
% # OrderID
% # DefaultFlag
% # SeqMode
% ### @return ###
% # TokenSeq
% # CardNoToken
% # Forward
% ### example ###
% # gmo.trade_brandtoken({
% #   member_id: 'mem10001',
% #   order_id: 'ord10001'
% # })
% # => {"TokenSeq"=>"0", "CardNoToken"=>"*************111", "Forward"=>"2a99663"}
traded_brandtoken(Client = ?valid_client, Params = #{member_id := <<_/binary>>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/TradedBrandtoken.idPass">>,
	request(Client, post, Name, Params);
traded_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id, order_id], Params}).

% ### @params ###
% # AccessID
% # AccessPass
% # OrderID
% # TokenType
% # Token
% # MemberID
% # SeqMode
% # TokenSeq
% # ClientField1
% # ClientField2
% # ClientField3
% ### @return ###
% # Status
% # OrderID
% # Forward
% # Approve
% # TranID
% # TranDate
% # ClientField1
% # ClientField2
% # ClientField3
% ### example ###
% # gmo.exec_tran_brandtoken({
% #   order_id: "597ae8c36120b23a3c00014e",
% #   access_id: "139f8ec33a07c55f406937c52ce4473d",
% #   access_pass: "2689b204d2c17192fa35f9269fa7e744",
% #   token_type: :apple_pay,
% #   token: <Base64 encoded payment data>,
% #   member_id: "mem10001"
% # })
% # => {"Status"=>"CAPTURE", "OrderID"=>"597ae8c36120b23a3c00014e", "Forward"=>"2a99663", "Approve"=>"5487394", "TranID"=>"1707281634111111111111771216", "TranDate"=>"20170728163453", "ClientField1"=>"Custom field value 1", "ClientField2"=>"Custom field value 2", "ClientField3"=>"Custom field value 3"}
exec_tran_brandtoken(Client = ?valid_client, Params0 = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	member_id := <<_/binary>>,
	order_id := <<_/binary>>
}) ->
	Name = <<"/payment/ExecTranBrandtoken.idPass">>,
	Params =
		case maps:find(token_type) of
			{ok, TokenType} when is_atom(TokenType) ->
				maps:put(token_type, gmo_pg_const:token_type(TokenType), Params0);
			{ok, TokenType} when is_binary(TokenType) ->
				Params0;
			error ->
				Params0
		end,
	request(Client, post, Name, Params);
exec_tran_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [access_id, access_pass, member_id, order_id], Params}).

%%%===================================================================
%%% Request API functions
%%%===================================================================

%% @private
request(Client = ?valid_client, Method, Name, InputParams) ->
	Params = merge_input_params(Client, InputParams),
	gmo_pg_request:new(Client, Method, Name, Params).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
merge_input_params(#{
	shop_id := ShopID,
	shop_pass := ShopPass,
	site_id := SiteID,
	site_pass := SitePass
}, Params) ->
	maps:merge(Params, #{
		shop_id => ShopID,
		shop_pass => ShopPass,
		site_id => SiteID,
		site_pass => SitePass
	}).

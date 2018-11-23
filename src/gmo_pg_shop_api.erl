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
-module(gmo_pg_shop_api).

%% Public API
-export([entry_tran/2]).
-export([entry_tran_cvs/2]).
-export([entry_tran_pay_easy/2]).
-export([entry_tran_linepay/2]).
-export([entry_tran_brandtoken/2]).
-export([exec_tran/2]).
-export([exec_tran_cvs/2]).
-export([exec_tran_pay_easy/2]).
-export([exec_tran_linepay/2]).
-export([exec_tran_brandtoken/2]).
-export([alter_tran/2]).
-export([change_tran/2]).
-export([change_tran_brandtoken/2]).
-export([void_tran_brandtoken/2]).
-export([sales_tran_brandtoken/2]).
-export([refund_tran_brandtoken/2]).
-export([search_trade/2]).
-export([search_trade_multi/2]).
%% Request API
-export([request/4]).

%% Macros
-define(valid_client, #{
	'__struct__' := gmo_pg_client,
	shop_id := <<_/binary>>,
	shop_pass := <<_/binary>>
}).

%%%===================================================================
%%% Public API functions
%%%===================================================================

% ## 2.1.2.1.取引登録
% # これ以降の決済取引で必要となる取引 ID と取引パスワードの発行を行い、取引を開始します。
% # ItemCode
% # Tax
% # TdFlag
% # TdTenantName
% ### @return ###
% # AccessID
% # AccessPass
% # ErrCode
% # ErrInfo
% ### example ###
% # gmo.entry_tran({
% #   order_id: 100,
% #   job_cd: "AUTH",
% #   amount: 100
% # })
% # {"AccessID"=>"a41d83f1f4c908baeda04e6dc03e300c", "AccessPass"=>"d72eca02e28c88f98b9341a33ba46d5d"}
entry_tran(Client = ?valid_client, Params = #{job_cd := <<"CHECK">>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/EntryTran.idPass">>,
	request(Client, post, Name, Params);
entry_tran(Client = ?valid_client, Params = #{amount := <<_/binary>>, job_cd := <<_/binary>>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/EntryTran.idPass">>,
	request(Client, post, Name, Params);
entry_tran(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, {'or', [
		job_cd,
		order_id
	], [
		amount,
		job_cd,
		order_id
	]}, Params}).

% # 【コンビニ払い】
% #  2.1.2.1. 取引登録
% #  これ以降の決済取引で必要となる取引IDと取引パスワードの発行を行い、取引を開始します。
entry_tran_cvs(Client = ?valid_client, Params = #{amount := <<_/binary>>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/EntryTranCvs.idPass">>,
	request(Client, post, Name, Params);
entry_tran_cvs(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [amount, order_id], Params}).

% # 【Pay-easy決済】
% #  5.1.2.1. 取引登録
% #  これ以降の決済取引で必要となる取引IDと取引パスワードの発行を行い、取引を開始します。
entry_tran_pay_easy(Client = ?valid_client, Params = #{amount := <<_/binary>>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/EntryTranPayEasy.idPass">>,
	request(Client, post, Name, Params);
entry_tran_pay_easy(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [amount, order_id], Params}).

% # 【LINE Pay決済】
% #  20.1.2.1. 取引登録
% #  これ以降の決済取引で必要となる取引IDと取引パスワードの発行を行い、取引を開始します。
entry_tran_linepay(Client = ?valid_client, Params = #{amount := <<_/binary>>, job_cd := <<_/binary>>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/EntryTranLinepay.idPass">>,
	request(Client, post, Name, Params);
entry_tran_linepay(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [amount, job_cd, order_id], Params}).

% ### @params ###
% # OrderID
% # JobCd
% # Amount
% # ItemCode
% # Tax
% ### @return ###
% # AccessID
% # AccessPass
% ### example ###
% # gmo.entry_tran_brandtoken({
% #   order_id: "ord12345",
% #   job_cd: "AUTH",
% #   item_code: "1000001",
% #   tax: "0001001",
% #   amount: 100
% # })
% # => {"AccessID"=>"139f8ec33a07c55f406937c52ce4473d", "AccessPass"=>"2689b204d2c17192fa35f9269fa7e744"}
entry_tran_brandtoken(Client = ?valid_client, Params = #{amount := <<_/binary>>, job_cd := <<_/binary>>, order_id := <<_/binary>>}) ->
	Name = <<"/payment/EntryTranBrandtoken.idPass">>,
	request(Client, post, Name, Params);
entry_tran_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [amount, job_cd, order_id], Params}).

% ## 2.2.2.2.決済実行
% # 指定されたサイトに会員を登録します。
% # return
% # ACS
% # OrderID
% # Forward
% # Method
% # PayTimes
% # Approve
% # TranID
% # TranDate
% # CheckString
% # ClientField1
% # ClientField2
% # ClientField3
% ### @return ###
% # ACS
% # OrderID
% # Forward
% # Method
% # PayTimes
% # Approve
% # TranID
% # CheckString
% # ClientField1
% # ClientField2
% # ClientField3
% ### example ###
% # gmo.exec_tran({
% #   order_id:      100,
% #   access_id:    "a41d83f1f4c908baeda04e6dc03e300c",
% #   access_pass:  "d72eca02e28c88f98b9341a33ba46d5d",
% #   method:        1,
% #   pay_times:     1,
% #   card_no:       "4111111111111111",
% #   expire:        "1405", #format YYMM
% #   client_field_1: "client_field1"
% # })
% # {"ACS"=>"0", "OrderID"=>"100", "Forward"=>"2a99662", "Method"=>"1", "PayTimes"=>"", "Approve"=>"6294780", "TranID"=>"1302160543111111111111192829", "TranDate"=>"20130216054346", "CheckString"=>"3e455a2168fefc90dbb7db7ef7b0fe82", "ClientField1"=>"client_field1", "ClientField2"=>"", "ClientField3"=>""}
exec_tran(Client = ?valid_client, Params0 = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	token := <<_/binary>>
}) ->
	Name = <<"/payment/ExecTran.idPass">>,
	Params1 = maybe_put_client_field_flg(Params0),
	Params2 = maybe_put_device_category(Params1),
	request(Client, post, Name, Params2);
exec_tran(Client = ?valid_client, Params0 = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	card_no := <<_/binary>>,
	expire := <<_/binary>>,
	order_id := <<_/binary>>
}) ->
	Name = <<"/payment/ExecTran.idPass">>,
	Params1 = maybe_put_client_field_flg(Params0),
	Params2 = maybe_put_device_category(Params1),
	request(Client, post, Name, Params2);
exec_tran(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, {'or', [
		access_id,
		access_pass,
		token
	], [
		access_id,
		access_pass,
		card_no,
		expire,
		order_id
	]}, Params}).

% # 【コンビニ払い】
% # 2.1.2.2. 決済実行
% # お客様が入力した情報で後続の決済センターと通信を行い決済を実施し、結果を返します。
exec_tran_cvs(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	convenience := <<_/binary>>,
	customer_kana := <<_/binary>>,
	customer_name := <<_/binary>>,
	order_id := <<_/binary>>,
	receipts_disp_11 := <<_/binary>>,
	receipts_disp_12 := <<_/binary>>,
	receipts_disp_13 := <<_/binary>>,
	tel_no := <<_/binary>>
}) ->
	Name = <<"/payment/ExecTranCvs.idPass">>,
	request(Client, post, Name, Params);
exec_tran_cvs(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		convenience,
		customer_kana,
		customer_name,
		order_id,
		receipts_disp_11,
		receipts_disp_12,
		receipts_disp_13,
		tel_no
	], Params}).

% # 【Pay-easy決済】
% # 5.1.2.2. 決済実行
% # お客様が入力した情報で後続の決済センターと通信を行い決済を実施し、結果を返します。
exec_tran_pay_easy(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	customer_kana := <<_/binary>>,
	customer_name := <<_/binary>>,
	order_id := <<_/binary>>,
	receipts_disp_11 := <<_/binary>>,
	receipts_disp_12 := <<_/binary>>,
	receipts_disp_13 := <<_/binary>>,
	tel_no := <<_/binary>>
}) ->
	Name = <<"/payment/ExecTranPayEasy.idPass">>,
	request(Client, post, Name, Params);
exec_tran_pay_easy(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		customer_kana,
		customer_name,
		order_id,
		receipts_disp_11,
		receipts_disp_12,
		receipts_disp_13,
		tel_no
	], Params}).

% # 【LINE Pay決済】
% # 20.1.2.2. 決済実行
exec_tran_linepay(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	error_rcv_url := <<_/binary>>,
	order_id := <<_/binary>>,
	product_name := <<_/binary>>,
	ret_url := <<_/binary>>
}) ->
	Name = <<"/payment/ExecTranLinepay.idPass">>,
	request(Client, post, Name, Params);
exec_tran_linepay(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		error_rcv_url,
		order_id,
		product_name,
		ret_url
	], Params}).

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
% #   seq_mode: "1",
% #   token_seq: 1001,
% #   client_field_1: "Custom field value 1",
% #   client_field_2: "Custom field value 2",
% #   client_field_3: "Custom field value 3"
% # })
% # => {"Status"=>"CAPTURE", "OrderID"=>"597ae8c36120b23a3c00014e", "Forward"=>"2a99663", "Approve"=>"5487394", "TranID"=>"1707281634111111111111771216", "TranDate"=>"20170728163453", "ClientField1"=>"Custom field value 1", "ClientField2"=>"Custom field value 2", "ClientField3"=>"Custom field value 3"}
exec_tran_brandtoken(Client = ?valid_client, Params0 = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
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
	erlang:error({missing_required_params, [access_id, access_pass, order_id], Params}).

% ## 2.14.2.1.決済変更
% # 仮売上の決済に対して実売上を行います。尚、実行時に仮売上時との金額チェックを行います。
% # /payment/AlterTran.idPass
% # ShopID
% # ShopPass
% # AccessID 取引ID
% # AccessPass 取引パスワード
% # JobCd 処理区分 "SALES"
% # Amount 利用金額
% ### @return ###
% # AccessID
% # AccessPass
% # Forward
% # Approve
% # TranID
% # TranDate
% ### example ###
% # gmo.alter_tran({
% #   access_id:    "a41d83f1f4c908baeda04e6dc03e300c",
% #   access_pass:  "d72eca02e28c88f98b9341a33ba46d5d",
% #   job_cd: "SALES",
% #   amount: 100
% # })
% # {"AccessID"=>"381d84ae4e6fc37597482573a9569f10", "AccessPass"=>"cc0093ca8758c6616fa0ab9bf6a43e8d", "Forward"=>"2a99662", "Approve"=>"6284199", "TranID"=>"1302140555111111111111193536", "TranDate"=>"20130215110651"}
alter_tran(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	job_cd := <<_/binary>>
}) ->
	Name = <<"/payment/AlterTran.idPass">>,
	request(Client, post, Name, Params);
alter_tran(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		job_cd
	], Params}).

% ## 2.15.2.1.金額変更
% # 決済が完了した取引に対して金額の変更を行います。
% ### @return ###
% # AccessID
% # AccessPass
% # Forward
% # Approve
% # TranID
% # TranDate
% ### example ###
% # gmo.change_tran({
% #   access_id:    "a41d83f1f4c908baeda04e6dc03e300c",
% #   access_pass:  "d72eca02e28c88f98b9341a33ba46d5d",
% #   job_cd: "CAPTURE",
% #   amount: 100
% # })
change_tran(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	amount := <<_/binary>>,
	job_cd := <<_/binary>>
}) ->
	Name = <<"/payment/ChangeTran.idPass">>,
	request(Client, post, Name, Params);
change_tran(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		amount,
		job_cd
	], Params}).

% ### @params ###
% # AccessID
% # AccessPass
% # OrderID
% # JobCd
% # Amount
% # Tax
% ### @return ###
% # AccessID
% # AccessPass
% # Status
% # Forward
% # Approve
% # TranID
% # TranDate
% ### example ###
% # gmo.change_tran_brandtoken({
% #   access_id: "21170701482c86c3b88ff72b83bfd363",
% #   access_pass: "51f36feba120de1e6e29532e5a3a5e3e",
% #   order_id: "ord10001",
% #   job_cd: "CAPTURE",
% #   amount: 2000
% # })
% # => {"AccessID"=>"21170701482c86c3b88ff72b83bfd363", "AccessPass"=>"51f36feba120de1e6e29532e5a3a5e3e", "Status"=>"CAPTURE", "Forward"=>"2a99663", "Approve"=>"5538477", "TranID"=>"1707311633111111111111771224", "TranDate"=>"20170731163343"}
change_tran_brandtoken(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	amount := <<_/binary>>,
	job_cd := <<_/binary>>,
	order_id := <<_/binary>>
}) ->
	Name = <<"/payment/ChangeTranBrandtoken.idPass">>,
	request(Client, post, Name, Params);
change_tran_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		amount,
		job_cd,
		order_id
	], Params}).

% ### @params ###
% # AccessID
% # AccessPass
% # OrderID
% ### @return ###
% # AccessID
% # AccessPass
% # Status
% # Forward
% # Approve
% # TranID
% # TranDate
% ### example ###
% # gmo.void_tran_brandtoken({
% #   access_id: "139f8ec33a07c55f406937c52ce4473d",
% #   access_pass: "2689b204d2c17192fa35f9269fa7e744",
% #   order_id: "597ae8c36120b23a3c00014e"
% # })
% # => {"AccessID"=>"139f8ec33a07c55f406937c52ce4473d", "AccessPass"=>"2689b204d2c17192fa35f9269fa7e744", "Status"=>"VOID", "Forward"=>"2a99663", "Approve"=>"5537590", "TranID"=>"1707311610111111111111771219", "TranDate"=>"20170731161007"}
void_tran_brandtoken(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	order_id := <<_/binary>>
}) ->
	Name = <<"/payment/VoidTranBrandtoken.idPass">>,
	request(Client, post, Name, Params);
void_tran_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		order_id
	], Params}).

% ### @params ###
% # AccessID
% # AccessPass
% # OrderID
% # Amount
% # Tax
% ### @return ###
% # AccessID
% # AccessPass
% # Status
% # Forward
% # Approve
% # TranID
% # TranDate
% ### example ###
% # gmo.sales_tran_brandtoken({
% #   access_id: "139f8ec33a07c55f406937c52ce4473d",
% #   access_pass: "2689b204d2c17192fa35f9269fa7e744",
% #   order_id: "597ae8c36120b23a3c00014e",
% #   amount: 1000,
% #   tax: "0001001"
% # })
% # => {"AccessID"=>"139f8ec33a07c55f406937c52ce4473d", "AccessPass"=>"2689b204d2c17192fa35f9269fa7e744", "Status"=>"SALES", "Forward"=>"2a99663", "Approve"=>"5537883", "TranID"=>"1707311620111111111111771220", "TranDate"=>"20170731162256"}
sales_tran_brandtoken(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	amount := <<_/binary>>,
	order_id := <<_/binary>>
}) ->
	Name = <<"/payment/SalesTranBrandtoken.idPass">>,
	request(Client, post, Name, Params);
sales_tran_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		amount,
		order_id
	], Params}).

% ### @params ###
% # AccessID
% # AccessPass
% # OrderID
% # Amount
% # Tax
% ### @return ###
% # AccessID
% # AccessPass
% # Status
% # Forward
% # Approve
% # TranID
% # TranDate
% ### example ###
% # gmo.refund_tran_brandtoken({
% #   access_id: "139f8ec33a07c55f406937c52ce4473d",
% #   access_pass: "2689b204d2c17192fa35f9269fa7e744",
% #   order_id: "597ae8c36120b23a3c00014e",
% #   amount: 1000,
% #   tax: "0001001"
% # })
% # => {"AccessID"=>"139f8ec33a07c55f406937c52ce4473d", "AccessPass"=>"2689b204d2c17192fa35f9269fa7e744", "Status"=>"RETURN", "Forward"=>"2a99663", "Approve"=>"5537883", "TranID"=>"1707311620111111111111771220", "TranDate"=>"20170731162256"}
refund_tran_brandtoken(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	amount := <<_/binary>>,
	order_id := <<_/binary>>
}) ->
	Name = <<"/payment/RefundTranBrandtoken.idPass">>,
	request(Client, post, Name, Params);
refund_tran_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		amount,
		order_id
	], Params}).

% ## 2.16.2.1.取引状態参照
% # 指定したオーダーID の取引情報を取得します。
search_trade(Client = ?valid_client, Params = #{order_id := <<_/binary>>}) ->
	Name = <<"/payment/SearchTrade.idPass">>,
	request(Client, post, Name, Params);
search_trade(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [order_id], Params}).

% # 13.1.2.1.取引状態参照
% # 指定したオーダーIDの取引情報を取得します。
% ### @params ###
% # OrderID
% # PayType
% ### @return ###
% # OrderID
% # Status
% # ProcessDate
% # JobCd
% # AccessID
% # AccessPass
% # ItemCode
% # Amount
% # Tax
% # SiteID
% # MemberID
% # CardNoToken
% # Expire
% # Method
% # PayTimes
% # Forward
% # TranID
% # Approve
% # ClientField1
% # ClientField2
% # ClientField3
% # PayType
% ### example ###
% # gmo.search_trade_multi({
% #   order_id: '598066176120b2235300020b',
% #   pay_type: 27
% # })
% # => {"OrderID"=>"598066176120b2235300020b", "Status"=>"CAPTURE", "ProcessDate"=>"20170801202929", "JobCd"=>"CAPTURE", "AccessID"=>"228fc5bc02da46943300c12706d325a2", "AccessPass"=>"090a50ec2f77d92184a18018f07906e5", "ItemCode"=>"0000990", "Amount"=>"557", "Tax"=>"0", "SiteID"=>"", "MemberID"=>"", "CardNoToken"=>"************1111", "Expire"=>"2212", "Method"=>"1", "PayTimes"=>"", "Forward"=>"2a99663", "TranID"=>"1708012029111111111111771228", "Approve"=>"5689128", "ClientField1"=>"", "ClientField2"=>"", "ClientField3"=>"", "PayType"=>"27"}
search_trade_multi(Client = ?valid_client, Params = #{order_id := <<_/binary>>, pay_type := <<_/binary>>}) ->
	Name = <<"/payment/SearchTradeMulti.idPass">>,
	request(Client, post, Name, Params);
search_trade_multi(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [order_id, pay_type], Params}).

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
maybe_put_client_field_flg(P = #{client_field_flg := <<_/binary>>}) ->
	P;
maybe_put_client_field_flg(P) when is_map(P) ->
	case maps:is_key(client_field_1, P) orelse maps:is_key(client_field_2, P) orelse maps:is_key(client_field_3, P) of
		true ->
			maps:put(client_field_flg, <<"1">>, P);
		false ->
			maps:put(client_field_flg, <<"0">>, P)
	end.

%% @private
maybe_put_device_category(P = #{device_category := <<_/binary>>}) ->
	P;
maybe_put_device_category(P) when is_map(P) ->
	maps:put(device_category, <<"0">>, P).

%% @private
merge_input_params(#{
	shop_id := ShopID,
	shop_pass := ShopPass
}, Params) ->
	maps:merge(Params, #{
		shop_id => ShopID,
		shop_pass => ShopPass
	}).

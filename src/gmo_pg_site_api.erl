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
-module(gmo_pg_site_api).

%% Public API
-export([save_member/2]).
-export([update_member/2]).
-export([delete_member/2]).
-export([search_member/2]).
-export([save_card/2]).
-export([delete_card/2]).
-export([search_card/2]).
-export([search_brandtoken/2]).
-export([delete_brandtoken/2]).
-export([exec_tran/2]).
-export([search_card_detail/2]).
%% Request API
-export([request/4]).

%% Macros
-define(valid_client, #{
	'__struct__' := gmo_pg_client,
	site_id := <<_/binary>>,
	site_pass := <<_/binary>>
}).

%%%===================================================================
%%% Public API functions
%%%===================================================================

% ## 2.3.2.1.会員登録
% # 指定されたサイトに会員を登録します。
% ### @params ###
% # MemberID
% ### @return ###
% # MemberID
% ### example ###
% # gmo.save_member({
% #   member_id: 'mem10001'
% # })
% # => {"MemberID"=>"mem10001"}
save_member(Client = ?valid_client, Params = #{member_id := <<_/binary>>}) ->
	Name = <<"/payment/SaveMember.idPass">>,
	request(Client, post, Name, Params);
save_member(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id], Params}).

% ## 2.4.2.1.会員更新
% # 指定されたサイトに会員情報を更新します。
update_member(Client = ?valid_client, Params = #{member_id := <<_/binary>>}) ->
	Name = <<"/payment/UpdateMember.idPass">>,
	request(Client, post, Name, Params);
update_member(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id], Params}).

% ## 2.5.2.1.会員削除
% # 指定したサイトから会員情報を削除します。
delete_member(Client = ?valid_client, Params = #{member_id := <<_/binary>>}) ->
	Name = <<"/payment/DeleteMember.idPass">>,
	request(Client, post, Name, Params);
delete_member(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id], Params}).

% ## 2.6.2.1.会員参照
% # 指定したサイトの会員情報を参照します。
search_member(Client = ?valid_client, Params = #{member_id := <<_/binary>>}) ->
	Name = <<"/payment/SearchMember.idPass">>,
	request(Client, post, Name, Params);
search_member(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id], Params}).

% ## 2.7.2.1.カード登録/更新
% # 指定した会員にカード情報を登録します。尚、サイトに設定されたショップ ID を使用してカード会社と通信を行い有効性の確認を行います。
save_card(Client = ?valid_client, Params = #{member_id := <<_/binary>>, token := <<_/binary>>}) ->
	Name = <<"/payment/SaveCard.idPass">>,
	request(Client, post, Name, Params);
save_card(Client = ?valid_client, Params = #{member_id := <<_/binary>>, card_no := <<_/binary>>, expire := <<_/binary>>}) ->
	Name = <<"/payment/SaveCard.idPass">>,
	request(Client, post, Name, Params);
save_card(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, {'or', [member_id, card_no, expire], [member_id, token]}, Params}).

% ## 2.8.2.1.カード削除
% # 指定した会員のカード情報を削除します。
delete_card(Client = ?valid_client, Params = #{member_id := <<_/binary>>, card_seq := <<_/binary>>}) ->
	Name = <<"/payment/DeleteCard.idPass">>,
	request(Client, post, Name, Params);
delete_card(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id, card_seq], Params}).

% ## 2.9.2.1.カード参照
% # 指定した会員のカード情報を参照します。
% # /payment/SearchCard.idPass
search_card(Client = ?valid_client, Params = #{member_id := <<_/binary>>, seq_mode := <<_/binary>>}) ->
	Name = <<"/payment/SearchCard.idPass">>,
	request(Client, post, Name, Params);
search_card(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id, seq_mode], Params}).

% ### @params ###
% # MemberID
% # SeqMode
% # TokenSeq
% ### @return ###
% # TokenSeq
% # DefaultFlag
% # CardName
% # CardNoToken
% # Expire
% # HolderName
% # DeleteFlag
% ### example ###
% # gmo.search_brandtoken({
% #   member_id: '598066176120b2235300020b',
% #   seq_mode: 0
% # })
% # => {"TokenSeq"=>"0", "DefaultFlag"=>"0", "CardName"=>"", "CardNoToken"=>"*************111", "Expire"=>"2212", "HolderName"=>"", "DeleteFlag"=>"0"}
search_brandtoken(Client = ?valid_client, Params = #{member_id := <<_/binary>>, seq_mode := <<_/binary>>}) ->
	Name = <<"/payment/SearchBrandtoken.idPass">>,
	request(Client, post, Name, Params);
search_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id, seq_mode], Params}).

% ### @params ###
% # MemberID
% # SeqMode
% # TokenSeq
% ### @return ###
% # TokenSeq
% ### example ###
% # gmo.delete_brandtoken({
% #   member_id: '598066176120b2235300020b',
% #   seq_mode: 0,
% #   token_seq: 0
% # })
% # => {"TokenSeq"=>"0"}
delete_brandtoken(Client = ?valid_client, Params = #{member_id := <<_/binary>>, seq_mode := <<_/binary>>, token_seq := <<_/binary>>}) ->
	Name = <<"/payment/DeleteBrandtoken.idPass">>,
	request(Client, post, Name, Params);
delete_brandtoken(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id, seq_mode, token_seq], Params}).

% ## 2.11.2.3. 決済実行
% # お客様が選択したカード登録連番のカード情報を取得します。
% # カード情報が本人認証サービスに対応していない場合は、カード会社との通信を行い決済を実行します。その際の出力パラメータは「2.10.2.3決済実行」の出力パラメータと同じになります。
% # /payment/ExecTran.idPass
exec_tran(Client = ?valid_client, Params = #{
	access_id := <<_/binary>>,
	access_pass := <<_/binary>>,
	card_seq := <<_/binary>>,
	member_id := <<_/binary>>,
	order_id := <<_/binary>>
}) ->
	Name = <<"/payment/ExecTran.idPass">>,
	request(Client, post, Name, Params);
exec_tran(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		access_id,
		access_pass,
		card_seq,
		member_id,
		order_id
	], Params}).

% ## 2.19.2.1.カード属性照会（サイトID+会員ID(+カード登録連番モード・カード登録連番)を指定して呼び出す場合）
% # 指定したクレジットカードの属性情報を取得します。
% # /payment/SearchCardDetail.idPass
search_card_detail(Client = ?valid_client, Params = #{member_id := <<_/binary>>, seq_mode := <<_/binary>>}) ->
	Name = <<"/payment/SearchCardDetail.idPass">>,
	request(Client, post, Name, Params);
search_card_detail(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [member_id, seq_mode], Params}).

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
	site_id := SiteID,
	site_pass := SitePass
}, Params) ->
	maps:merge(Params, #{
		site_id => SiteID,
		site_pass => SitePass
	}).

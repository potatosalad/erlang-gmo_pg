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
-module(gmo_pg_remittance_api).

%% Public API
-export([create_account/2]).
-export([update_account/2]).
-export([delete_account/2]).
-export([search_account/2]).
-export([create_deposit/2]).
-export([cancel_deposit/2]).
-export([search_deposit/2]).
-export([search_balance/2]).
-export([create_mail_deposit/2]).
-export([cancel_mail_deposit/2]).
-export([search_mail_deposit/2]).
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

% #########
% # Method
% # Bank_ID
% # Bank_Code
% # Branch_Code
% # Account_Type
% # Account_Name
% # Account_Number
% # Branch_Code_Jpbank
% # Account_Number_Jpbank
% # Free
% ### @return ###
% # Bank_ID
% # Method
% # ErrCode
% # ErrInfo
% ### example ###
% # gmo.create_account({
% #   bank_id:           'bank00000',
% #   bank_code:         '0001',
% #   branch_code:       '813',
% #   account_type:      :normal,
% #   account_name:      'An Yutzy',
% #   account_number:    '0012345',
% #   branch_code_jp:    '00567',
% #   account_number_jp: '01234567',
% #   free:              'foobar'      # Metadata
% # })
% # {"Bank_ID"=>"bank00000", "Method"=>"1"}
create_account(Client = ?valid_client, Params0 = #{
	account_name := <<_/binary>>,
	account_number := <<_/binary>>,
	account_type := AccountType,
	bank_code := <<_/binary>>,
	bank_id := <<_/binary>>,
	branch_code := <<_/binary>>
}) ->
	Name = <<"/api/AccountRegistration.idPass">>,
	Params1 =
		case maps:is_key(method, Params0) of
			false ->
				maps:put(method, <<"1">>, Params0);
			true ->
				Params0
		end,
	Params2 =
		case AccountType of
			_ when is_atom(AccountType) ->
				maps:put(account_type, gmo_pg_const:account_type(AccountType), Params1);
			<<_/binary>> ->
				Params1
		end,
	request(Client, post, Name, Params2);
create_account(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		account_name,
		account_number,
		account_type,
		bank_code,
		bank_id,
		branch_code
	], Params}).

% #########
% # Method
% # Bank_ID
% # Bank_Code
% # Branch_Code
% # Account_Type
% # Account_Name
% # Account_Number
% # Branch_Code_Jpbank
% # Account_Number_Jpbank
% # Free
% ### @return ###
% # Bank_ID
% # Method
% # ErrCode
% # ErrInfo
% ### example ###
% # gmo.update_account({
% #   bank_id:           'bank00000',
% #   bank_code:         '0001',
% #   branch_code:       '813',
% #   account_type:      :normal,
% #   account_name:      'An Yutzy',
% #   account_number:    '0012345',
% #   branch_code_jp:    '00567',
% #   account_number_jp: '01234567',
% #   free:              'foobar'      # Metadata
% # })
% # {"Bank_ID"=>"bank00000", "Method"=>"2"}
update_account(Client = ?valid_client, Params0 = #{
	account_name := <<_/binary>>,
	account_number := <<_/binary>>,
	account_type := AccountType,
	bank_code := <<_/binary>>,
	bank_id := <<_/binary>>,
	branch_code := <<_/binary>>
}) ->
	Name = <<"/api/AccountRegistration.idPass">>,
	Params1 =
		case maps:is_key(method, Params0) of
			false ->
				maps:put(method, <<"2">>, Params0);
			true ->
				Params0
		end,
	Params2 =
		case AccountType of
			_ when is_atom(AccountType) ->
				maps:put(account_type, gmo_pg_const:account_type(AccountType), Params1);
			<<_/binary>> ->
				Params1
		end,
	request(Client, post, Name, Params2);
update_account(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		account_name,
		account_number,
		account_type,
		bank_code,
		bank_id,
		branch_code
	], Params}).

% #########
% # Method
% # Bank_ID
% ### @return ###
% # Bank_ID
% # Method
% # ErrCode
% # ErrInfo
% ### example ###
% # gmo.delete_account({
% #   bank_id: 'bank00000',
% # })
% # {"Bank_ID"=>"bank00000", "Method"=>"3"}
delete_account(Client = ?valid_client, Params0 = #{
	bank_id := <<_/binary>>
}) ->
	Name = <<"/api/AccountRegistration.idPass">>,
	Params1 =
		case maps:is_key(method, Params0) of
			false ->
				maps:put(method, <<"3">>, Params0);
			true ->
				Params0
		end,
	request(Client, post, Name, Params1);
delete_account(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		bank_id
	], Params}).

% #########
% # Bank_ID
% ### @return ###
% # Bank_ID
% # Delete_Flag
% # Bank_Name
% # Bank_Code
% # Branch_Name
% # Branch_Code
% # Account_Type
% # Account_Number
% # Account_Name
% # Free
% # Branch_Code_Jpbank
% # Account_Number_Jpbank
% ### example ###
% # gmo.search_account({
% #   bank_id: 'bank12345'
% # })
% # {"Bank_ID"=>"bank12345", "Delete_Flag"=>"0", "Bank_Name"=>"みずほ銀行", "Bank_Code"=>"0001", "Branch_Name"=>"札幌支店", "Branch_Code"=>"813", "Account_Type"=>"1", "Account_Number"=>"0012345", "Account_Name"=>"An Yutzy", "Free"=>"", "Branch_Code_Jpbank"=>"", "Account_Number_Jpbank"=>""}
search_account(Client = ?valid_client, Params = #{
	bank_id := <<_/binary>>
}) ->
	Name = <<"/api/AccountSearch.idPass">>,
	request(Client, post, Name, Params);
search_account(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		bank_id
	], Params}).

% #########
% # Method
% # Deposit_ID
% # Bank_ID
% # Amount
% ### @return ###
% # Deposit_ID
% # Bank_ID
% # Method
% # Amount
% # Bank_Fee
% ### example ###
% # gmo.create_deposit({
% #   deposit_id: 'dep00000',
% #   bank_id:    'bank00000',
% #   amount:     '1000'
% # })
% # {"Deposit_ID"=>"dep00000", "Bank_ID"=>"bank00000", "Method"=>"1", "Amount"=>"1000", "Bank_Fee"=>"27"}
create_deposit(Client = ?valid_client, Params0 = #{
	amount := <<_/binary>>,
	bank_id := <<_/binary>>,
	deposit_id := <<_/binary>>
}) ->
	Name = <<"/api/DepositRegistration.idPass">>,
	Params1 =
		case maps:is_key(method, Params0) of
			false ->
				maps:put(method, <<"1">>, Params0);
			true ->
				Params0
		end,
	request(Client, post, Name, Params1);
create_deposit(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		amount,
		bank_id,
		deposit_id
	], Params}).

% #########
% # Method
% # Deposit_ID
% # Bank_ID
% ### @return ###
% # Deposit_ID
% # Bank_ID
% # Method
% ### example ###
% # gmo.cancel_deposit({
% #   deposit_id: 'dep00000',
% #   bank_id:    'bank00000',
% # })
% # {"Deposit_ID"=>"dep00000", "Bank_ID"=>"bank00000", "Method"=>"2"}
cancel_deposit(Client = ?valid_client, Params0 = #{
	bank_id := <<_/binary>>,
	deposit_id := <<_/binary>>
}) ->
	Name = <<"/api/DepositRegistration.idPass">>,
	Params1 =
		case maps:is_key(method, Params0) of
			false ->
				maps:put(method, <<"2">>, Params0);
			true ->
				Params0
		end,
	request(Client, post, Name, Params1);
cancel_deposit(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		bank_id,
		deposit_id
	], Params}).

% #############
% # Deposit_ID
% ### @return ###
% # Deposit_ID
% # Bank_ID
% # Bank_Name
% # Bank_Code
% # Branch_Name
% # Branch_Code
% # Account_Type
% # Account_Number
% # Account_Name
% # Free
% # Amount
% # Bank_Fee
% # Result
% # Branch_Code_Jpbank
% # Account_Number_Jpbank
% # Deposit_Date
% # Result_Detail
% ### example ###
% # gmo.search_deposit({
% #   deposit_id: 'dep00000'
% # })
% # {"Deposit_ID"=>"dep00000", "Bank_ID"=>"bank163144", "Bank_Name"=>"みずほ銀行", "Bank_Code"=>"0001", "Branch_Name"=>"札幌支店", "Branch_Code"=>"813", "Account_Type"=>"1", "Account_Number"=>"0012345", "Account_Name"=>"An Yutzy", "Free"=>"", "Amount"=>"181035", "Bank_Fee"=>"270", "Result"=>"0", "Branch_Code_Jpbank"=>"", "Account_Number_Jpbank"=>"", "Deposit_Date"=>"", "Result_Detail"=>""}
search_deposit(Client = ?valid_client, Params = #{
	deposit_id := <<_/binary>>
}) ->
	Name = <<"/api/DepositSearch.idPass">>,
	request(Client, post, Name, Params);
search_deposit(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		deposit_id
	], Params}).

% #########
% ### @return ###
% # Shop_ID
% # Balance
% # Balance_Forecast
% ### example ###
% # gmo.search_balance
% # {"Shop_ID"=>"rshop00000071", "Balance"=>"9818965", "Balance_Forecast"=>"9818965"}
search_balance(Client = ?valid_client, Params = #{}) ->
	Name = <<"/api/BalanceSearch.idPass">>,
	request(Client, post, Name, Params);
search_balance(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [], Params}).

% ###########
% # Method
% # Deposit_ID
% # Mail_Address
% # Amount
% # Mail_Deposit_Account_Name
% # Expire
% # Shop_Mail_Address
% ### @return ###
% # Method
% # Amount
% # Deposit_ID
% # Expire
% ### example ###
% # gmo.create_mail_deposit({
% #   deposit_id: 'dep00001',
% #   deposit_email: 'anyutzy@demo.com',
% #   amount: 1000,
% #   deposit_account_name: 'An Yutzy',
% #   expire: 5,
% #   deposit_shop_email: 'anyutzy@demo.com'
% # })
% # {"Deposit_ID"=>"dep00009", "Method"=>"1", "Amount"=>"1200", "Expire"=>"20170503"}
create_mail_deposit(Client = ?valid_client, Params0 = #{
	amount := <<_/binary>>,
	deposit_account_name := <<_/binary>>,
	deposit_email := <<_/binary>>,
	deposit_id := <<_/binary>>,
	deposit_shop_email := <<_/binary>>,
	expire := <<_/binary>>
}) ->
	Name = <<"/api/MailDepositRegistration.idPass">>,
	Params1 =
		case maps:is_key(method, Params0) of
			false ->
				maps:put(method, <<"1">>, Params0);
			true ->
				Params0
		end,
	request(Client, post, Name, Params1);
create_mail_deposit(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		amount,
		deposit_account_name,
		deposit_email,
		deposit_id,
		deposit_shop_email,
		expire
	], Params}).

% ###########
% # Method
% # Deposit_ID
% ### @return ###
% # Deposit_ID
% # Method
% ### example ###
% # gmo.cancel_mail_deposit({
% #   deposit_id: 'dep00001',
% # })
% # {"Deposit_ID"=>"dep00001", "Method"=>"2"}
cancel_mail_deposit(Client = ?valid_client, Params0 = #{
	deposit_id := <<_/binary>>
}) ->
	Name = <<"/api/MailDepositRegistration.idPass">>,
	Params1 =
		case maps:is_key(method, Params0) of
			false ->
				maps:put(method, <<"2">>, Params0);
			true ->
				Params0
		end,
	request(Client, post, Name, Params1);
cancel_mail_deposit(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		deposit_id
	], Params}).

% #########
% # Deposit_ID
% ### @return ###
% # Deposit_ID
% # Mail_Address
% # Shop_Mail_Address
% # Account_Name
% # Amount
% # Expire
% # Status
% ### example ###
% # gmo.search_mail_deposit({
% #   deposit_id: 'dep00001'
% # })
% # {"Deposit_ID"=>"dep0001516", "Mail_Address"=>"anyutzy@demo.com", "Shop_Mail_Address"=>"anyutzy@demo.com", "Account_Name"=>"An Yutzy", "Amount"=>"1000", "Expire"=>"20170503", "Status"=>"0"}
search_mail_deposit(Client = ?valid_client, Params = #{
	deposit_id := <<_/binary>>
}) ->
	Name = <<"/api/MailDepositSearch.idPass">>,
	request(Client, post, Name, Params);
search_mail_deposit(_Client = ?valid_client, Params) ->
	erlang:error({missing_required_params, [
		deposit_id
	], Params}).

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
	shop_pass := ShopPass
}, Params) ->
	maps:merge(Params, #{
		shop_id => ShopID,
		shop_pass => ShopPass
	}).

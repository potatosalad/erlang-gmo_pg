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
-module(gmo_pg_util).

%% API
-export([list_join/2]).
-export([lowercase/1]).

%% Macros
-define(LC(C),
	case C of
		$A -> $a;
		$B -> $b;
		$C -> $c;
		$D -> $d;
		$E -> $e;
		$F -> $f;
		$G -> $g;
		$H -> $h;
		$I -> $i;
		$J -> $j;
		$K -> $k;
		$L -> $l;
		$M -> $m;
		$N -> $n;
		$O -> $o;
		$P -> $p;
		$Q -> $q;
		$R -> $r;
		$S -> $s;
		$T -> $t;
		$U -> $u;
		$V -> $v;
		$W -> $w;
		$X -> $x;
		$Y -> $y;
		$Z -> $z;
		_ -> C
	end).

%%%===================================================================
%%% API functions
%%%===================================================================

list_join(List, Sep) when is_list(List) ->
	list_join(List, Sep, <<>>).

lowercase(List) when is_list(List) ->
	lowercase(erlang:iolist_to_binary(List));
lowercase(Binary) when is_binary(Binary) ->
	<< << ?LC(C) >> || << C >> <= Binary >>.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
list_join([H], _Sep, Acc) ->
	<< Acc/binary, H/binary >>;
list_join([H | T], Sep, Acc) ->
	list_join(T, Sep, << Acc/binary, H/binary, Sep >>);
list_join([], _Sep, Acc) ->
	Acc.

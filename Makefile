PROJECT = gmo_pg
PROJECT_DESCRIPTION = GMO-PG Payment Gateway Client for Erlang and Elixir
PROJECT_VERSION = 0.0.0

DEPS = hackney iconv

dep_iconv = git git://github.com/processone/iconv.git master

include erlang.mk

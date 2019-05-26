%%%-------------------------------------------------------------------
%%% @author kamsza
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. maj 2019 18:48
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-author("kamsza").
-behaviour(supervisor).

-compile(export_all).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok, {
    {one_for_one, 2, 3},
    [{pollution_server, {pollution_server, start_link, []}, permanent, 1000, worker, [pollution_server]}]
  }}.

stop() ->
  supervisor:terminate_child(?MODULE, pollution_server),
  supervisor:delete_child(?MODULE, pollution_server),
  exit(self(), normal).
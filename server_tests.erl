%%%-------------------------------------------------------------------
%%% @author kamsza
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. maj 2019 17:24
%%%-------------------------------------------------------------------
-module(server_tests).
-author("kamsza").

-include_lib("eunit/include/eunit.hrl").

server_test() ->
  pollution_supervisor:start_link(),
  ?assertEqual(ok,pollution_server:addStation("A", {0,0})),
  ?assertEqual(ok,pollution_server:addStation("B", {1,0})),
  ?assertEqual(ok,pollution_server:addStation("C", {2,0})),
  ?assertEqual(ok,pollution_server:addStation("D", {3,0})),
  ?assertNotEqual(ok,pollution_server:addStation("A", {1,0})),
  ?assertNotEqual(ok,pollution_server:addStation("Q", {0,0})),
  ?assertEqual(ok,pollution_server:addValue("A", {{2018,05,01},{15,20,12}},"PM10",100)),
  ?assertEqual(ok,pollution_server:addValue("A", {{2018,05,01},{16,20,12}},"PM10",80)),
  ?assertEqual(ok,pollution_server:addValue("A", {{2018,05,01},{17,20,12}},"PM10",60)),
  ?assertEqual(ok,pollution_server:addValue("A", {{2018,05,01},{18,20,12}},"PM10",40)),
  ?assertEqual(ok,pollution_server:addValue("A", {{2018,05,01},{19,20,12}},"PM2.5",130)),
  ?assertEqual(ok,pollution_server:addValue("D", {{2018,05,01},{19,20,12}},"PM2.5",150)),
  ?assertNotEqual(ok,pollution_server:addValue("A", {{2018,05,01},{19,20,12}},"PM2.5",130)),
  ?assertEqual(ok,pollution_server:addValue("C", {{2018,05,01},{19,20,12}},"PM2.5",110)),
  ?assertNotEqual(ok,pollution_server:addValue("Q", {{2018,05,01},{19,20,12}},"PM2.5",130)),
  ?assertEqual(ok,pollution_server:removeValue({1,0}, {2018,05,01},"PM2.5")),
  ?assertNotEqual(ok,pollution_server:removeValue({7,0}, {2018,05,01},"PM2.5")),
  ?assertEqual(70.0, pollution_server:getStationMean("A", "PM10")),
  ?assertEqual(0, pollution_server:getStationMean("C", "PM10")),
  ?assertEqual(70.0, pollution_server:getDailyMean("PM10", {2018,05,01})),
  ?assertEqual(130.0, pollution_server:getDailyMean("PM2.5", {2018,05,01})),
  ?assertEqual(130, pollution_server:getOneValue("A", {2018,05,01}, "PM2.5")),
  pollution_supervisor:stop().
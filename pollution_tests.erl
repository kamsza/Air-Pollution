%%%-------------------------------------------------------------------
%%% @author kamsz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2019 16:36
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("kamsz").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

createMonitor_test()->
  ?assert(pollution:createMonitor()=:={monitor,#{},#{},#{}}).

addStation_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Stacja1",{0,0},P),
  P2 = pollution:addStation("Stacja1",{100,12},P1),
  P3 = pollution:addStation("StacjaNie1",{0,0},P1),
  P4 = pollution:addStation("Stacja1",{0,0},P1),
  P5 = pollution:addStation("Stacja2",{1,1},P1),
  ?assertEqual({monitor,#{"Stacja1" => {station,"Stacja1",{0,0}}},
    #{{0,0} => {station,"Stacja1",{0,0}}},
    #{{station,"Stacja1",{0,0}} => []}}, P1),
  ?assertEqual({error, "Similar station already exists"},P2),
  ?assertEqual({error, "Similar station already exists"},P3),
  ?assertEqual({error, "Similar station already exists"},P4),
  ?assertEqual({monitor,#{"Stacja1" => {station,"Stacja1",{0,0}},
    "Stacja2" => {station,"Stacja2",{1,1}}},
    #{{0,0} => {station,"Stacja1",{0,0}},
      {1,1} => {station,"Stacja2",{1,1}}},
    #{{station,"Stacja1",{0,0}} => [],
      {station,"Stacja2",{1,1}} => []}}, P5).

addValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Stacja",{0,0}, P),
  P2 = pollution:addValue("Stacja",{{2018,05,01},{15,20,12}},"PM10",70,P1),
  P3 = pollution:addValue({0,0},{{2018,05,02},{15,20,12}},"PM10",100,P2),
  P4 = pollution:addValue({0,0},{{2018,05,02},{15,21,12}},"PM2.5",80,P3),
  P5 = pollution:addValue("Stacja",{{2018,05,02},{15,20,12}},"PM10", 800,P4),
  P6 = pollution:addValue({0,0},{{2018,05,02},{15,21,12}},"PM2.5", 1200,P4),
  P7 = pollution:addValue("Stacja1",{{2018,05,02},{15,20,12}},"PM2.5", 80,P4),
  ?assertEqual({monitor,#{"Stacja" => {station,"Stacja",{0,0}}},
    #{{0,0} => {station,"Stacja",{0,0}}},
    #{{station,"Stacja",{0,0}} => []}}, P1),
  ?assertEqual({monitor,#{"Stacja" => {station,"Stacja",{0,0}}},
    #{{0,0} => {station,"Stacja",{0,0}}},
    #{{station,"Stacja",{0,0}} =>
    [{measurement,{{2018,5,1},{15,20,12}},"PM10",70}]}}, P2),
  ?assertEqual({monitor,#{"Stacja" => {station,"Stacja",{0,0}}},
    #{{0,0} => {station,"Stacja",{0,0}}},
    #{{station,"Stacja",{0,0}} =>
    [{measurement,{{2018,5,2},{15,20,12}},"PM10",100},{measurement,{{2018,5,1},{15,20,12}},"PM10",70}]}}, P3),
  ?assertEqual({monitor,#{"Stacja" => {station,"Stacja",{0,0}}},
    #{{0,0} => {station,"Stacja",{0,0}}},
    #{{station,"Stacja",{0,0}} =>
    [{measurement,{{2018,5,2},{15,21,12}},"PM2.5",80},
      {measurement,{{2018,5,2},{15,20,12}},"PM10",100},
      {measurement,{{2018,5,1},{15,20,12}},"PM10",70}]}}, P4),
  ?assertEqual({error, "Measurement with given data is already assigned to this station"}, P5),
  ?assertEqual({error, "Measurement with given data is already assigned to this station"}, P6),
  ?assertEqual({error, "Station don't exists"}, P7).

removeValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Stacja",{0,0}, P),
  P2 = pollution:addValue("Stacja",{{2018,05,01},{15,20,12}},"PM10",70,P1),
  P3 = pollution:addValue({0,0},{{2018,05,02},{15,20,12}},"PM10",100,P2),
  P4 = pollution:addValue({0,0},{{2018,05,02},{15,21,12}},"PM2.5",80,P3),
  P5 = pollution:removeValue("Stacja", {{2018,05,02},{15,20,12}},"PM10", P4),
  P6 = pollution:removeValue({0,0},{{2018,05,02},{15,21,12}},"PM2.5", P5),
  P7 = pollution:removeValue({1,1}, {{2018,05,02},{15,20,12}},"PM10", P6),
  P8 = pollution:removeValue("Stacja", {{2018,05,02},{15,21,12}},"PM2.5", P6),
  P9 = pollution:addStation("Stacja2",{1,1}, P2),
  P10 = pollution:addValue({1,1},{{2018,05,02},{17,20,12}},"PM2,5",100,P9),
  P11 = pollution:addValue({1,1},{{2018,05,02},{16,20,12}},"PM10",100,P10),
  P12 = pollution:addValue({1,1},{{2018,05,02},{15,20,12}},"PM10",100,P11),
  P13 = pollution:removeValue("Stacja2", {2018,05,02},"PM10", P12),
  ?assertEqual(P6, P2),
  ?assertEqual(P8, P2),
  ?assertEqual({error, "Station don't exists"}, P7),
  ?assertEqual(P13, P10).


getOneValue_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation( "Stacja1", {52, 32}, P1),
  P3= pollution:addValue( "Stacja1", {{2018,5,4},{21,22,39}}, "PM2,5", 10.0 , P2),
  ?assertEqual("Value not found", pollution:getOneValue("Stacja1", {{2024,5,4},{21,22,39}}, "PM2,5", P3)),
  ?assertEqual({error, "Station don't exists"}, pollution:getOneValue({1,1}, {{2018,5,4},{21,22,39}}, "PM2,5", P3)),
  ?assertEqual(10.0,pollution:getOneValue("Stacja1",{2018,5,4}, "PM2,5",P3)).

getStationMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Stacja pierwsza", {0,0},P),
  P2 = pollution:addStation("Stacja druga",{1,1},P1),
  P3 = pollution:addValue("Stacja pierwsza", {{2018,5,12},{12,47,58}}, "PM10", 4,P2),
  P4 = pollution:addValue("Stacja pierwsza", {{2018,5,13},{12,47,58}}, "PM10", 8,P3),
  P5 = pollution:addValue("Stacja pierwsza", {{2018,5,14},{12,47,58}}, "PM10", 3,P4),
  P6 = pollution:addValue("Stacja druga", {{2018,5,14},{21,47,58}}, "PM10", 2, P5),
  P7 = pollution:addStation("Stacja trzecia",{1,2},P6),
  ?assertEqual(5.0, pollution:getStationMean("Stacja pierwsza", "PM10",P7)),
  ?assertEqual(2.0, pollution:getStationMean("Stacja druga", "PM10",P7)),
  ?assertEqual(0, pollution:getStationMean("Stacja druga", "PM2,5",P7)),
  ?assertEqual(0, pollution:getStationMean("Stacja trzecia", "PM10",P7)),
  ?assertEqual({error, "Station don't exists"}, pollution:getStationMean("Stacja czwarta", "PM10",P7)).

getDailyMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Stacja pierwsza", {0,0},P),
  P2 = pollution:addStation("Stacja druga",{1,1},P1),
  P3 = pollution:addValue("Stacja pierwsza", {{2018,5,12},{12,47,58}}, "PM10", 4,P2),
  P4 = pollution:addValue("Stacja pierwsza", {{2018,5,12},{14,17,58}}, "PM10", 8,P3),
  P5 = pollution:addValue("Stacja pierwsza", {{2018,5,12},{15,47,58}}, "PM10", 3,P4),
  P6 = pollution:addValue("Stacja druga", {{2018,5,12},{21,47,58}}, "PM10", 2, P5),
  P7 = pollution:addStation("Stacja trzecia",{1,2},P6),
  P8 = pollution:addValue("Stacja trzecia", {{2018,5,12},{22,47,58}}, "PM10", 3, P7),
  ?assertEqual(4.0, pollution:getDailyMean("PM10",{2018,5,12}, P8)).

getDailyAverageDataCount_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Stacja pierwsza", {0,0},P),
  P2 = pollution:addStation("Stacja druga",{1,1},P1),
  P3 = pollution:addValue("Stacja pierwsza", {{2018,5,12},{12,47,58}}, "PM10", 4,P2),
  P4 = pollution:addValue("Stacja pierwsza", {{2018,5,12},{14,17,58}}, "PM10", 8,P3),
  P5 = pollution:addValue("Stacja pierwsza", {{2018,5,12},{15,47,58}}, "PM2.5", 3,P4),
  P6 = pollution:addValue("Stacja druga", {{2018,5,12},{21,47,58}}, "PM10", 2, P5),
  P7 = pollution:addStation("Stacja trzecia",{1,2},P6),
  P8 = pollution:addValue("Stacja trzecia", {{2018,5,12},{22,47,58}}, "PM10", 3, P7),
  P9 = pollution:addValue("Stacja trzecia", {{2018,5,12},{23,47,58}}, "PM2.5", 3, P8),
  P10 = pollution:addValue("Stacja trzecia", {{2018,4,13},{20,47,58}}, "PM10", 3, P9),
  P11 = pollution:addValue("Stacja trzecia", {{2018,4,13},{21,47,58}}, "PM10", 3, P10),
  P12 = pollution:addValue("Stacja trzecia", {{2018,4,13},{22,47,58}}, "PM10", 3, P11),
  ?assertEqual(2.0, pollution:getDailyAverageDataCount({2018,5,12}, P12)),
  ?assertEqual(1.0, pollution:getDailyAverageDataCount({2018,4,13}, P12)),
  ?assertEqual(0.0, pollution:getDailyAverageDataCount({2018,4,14}, P12)).
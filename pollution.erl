%%%-------------------------------------------------------------------
%%% @author kamsz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. mar 2019 21:31
%%%-------------------------------------------------------------------
-module(pollution).
-author("kamsz").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDayValues/3, getDailyMean/3, getDailyAverageDataCount/2, create/0]).

-record(station, {name, location}).
-record(measurement, {date, type, value}).
-record(monitor, {name, location, data}).
% rekord monitor zawiera trzy mapy
% name: name => station
% location: location => station
% data: station => measurement


createMonitor() -> #monitor{name = maps:new(), location = maps:new(), data = maps:new()}.

addStation(Name, {X,Y}, #monitor{} = Monitor) ->
  case maps:is_key(Name, Monitor#monitor.name) or maps:is_key({X, Y}, Monitor#monitor.location) of
    true -> {error, "Similar station already exists"};
    _ -> #monitor{
      name = (Monitor#monitor.name)#{Name => #station{name = Name, location = {X, Y}}},
      location = (Monitor#monitor.location)#{{X, Y} => #station{name = Name, location = {X, Y}}},
      data = (Monitor#monitor.data)#{#station{name = Name, location = {X, Y}} => []}
    }
  end.

addValue(Id, Date, Type, Value, #monitor{} = Monitor) ->
  case Id of
    {_,_} -> case maps:find(Id, Monitor#monitor.location) of
               {ok, Station} -> addValue(Station, #measurement{date = Date, type = Type, value = Value}, Monitor);
               error -> {error, "Station don't exists"}
             end;
    _ ->     case maps:find(Id, Monitor#monitor.name) of
               {ok, Station} -> addValue(Station, #measurement{date = Date, type = Type, value = Value}, Monitor);
               error -> {error, "Station don't exists"}
             end
  end.

addValue(Station, Measurement, Monitor) ->
  {_, List} = maps:find(Station, Monitor#monitor.data),
  case lists:any(fun(X) -> X#measurement.type == Measurement#measurement.type andalso  X#measurement.date == Measurement#measurement.date end, List) of
    true -> {error, "Measurement with given data is already assigned to this station"};
    false -> #monitor {
      name = (Monitor#monitor.name),
      location = (Monitor#monitor.location),
      data = (Monitor#monitor.data)#{Station := [Measurement | maps:get(Station, Monitor#monitor.data)]}
    }
  end.

removeValue(Id, Date, Type, Monitor) ->
  case Id of
    {_,_} -> case maps:find(Id, Monitor#monitor.location) of
               {ok, Station} -> removeValueS(Station, Date, Type, Monitor);
               error -> {error, "Station don't exists"}
             end;
    _ ->     case maps:find(Id, Monitor#monitor.name) of
               {ok, Station} -> removeValueS(Station, Date, Type, Monitor);
               error -> {error, "Station don't exists"}
             end
  end.

removeValueS(Station, {Date, Hour}, Type, Monitor) when is_record(Station, station) ->
  Measurements =  maps:get(Station, Monitor#monitor.data),
  NewMeasurements = [{M,{D,H},T,V} || {M,{D,H},T,V} <- Measurements, D /= Date orelse H /= Hour orelse  T /= Type],
  #monitor {
    name = (Monitor#monitor.name),
    location = (Monitor#monitor.location),
    data = (Monitor#monitor.data)#{Station := NewMeasurements}
  };

removeValueS(Station, Date, Type, Monitor) when is_record(Station, station) ->
  Measurements =  maps:get(Station, Monitor#monitor.data),
  NewMeasurements = [{M,{D,H},T,V} || {M,{D,H},T,V} <- Measurements, D /= Date orelse T /= Type],
  #monitor {
    name = (Monitor#monitor.name),
    location = (Monitor#monitor.location),
    data = (Monitor#monitor.data)#{Station := NewMeasurements}
  }.


getOneValue(Id, Date, Type, Monitor) ->
  case Id of
    {_,_} -> case maps:find(Id, Monitor#monitor.location) of
               {ok, Station} ->
                 Measurements = maps:find(Station, Monitor#monitor.data),
                 K = [{V#measurement.value, V#measurement.date} || V <- Measurements, V#measurement.type == Type],
                 [X || {X,{D,_}} <- K, D == Date];

               error -> {error, "Station don't exists"} end;
    _ ->     case maps:find(Id, Monitor#monitor.name) of
               {ok, Station} ->
                 {ok, Measurements} = maps:find(Station, Monitor#monitor.data),
                 Measurements,
                 K = [{V#measurement.value, V#measurement.date} || V <- Measurements, V#measurement.type == Type],
                 V = [X || {X,{D,_}} <- K, D == Date],
                 case V of
                   [] -> "Value not found";
                   [F | _] -> F
                 end;
               error -> {error, "Station don't exists"} end
  end.

getStationMean(Id, Type, Monitor) ->
  case Id of
    {_,_} -> case maps:find(Id, Monitor#monitor.location) of
               {ok, Station} ->
                 Type_list = [X || X <- maps:get(Station, Monitor#monitor.data), X#measurement.type == Type],
                 if
                   length(Type_list) == 0 -> 0;
                   true ->
                     Sum = lists:foldl(fun(X, S) -> X#measurement.value + S end, 0, Type_list),
                     Sum / length(Type_list)
                 end;
               error -> {error, "Station don't exists"}
             end;
    _ ->     case maps:find(Id, Monitor#monitor.name) of
               {ok, Station} ->
                 Type_list = [X || X <- maps:get(Station, Monitor#monitor.data), X#measurement.type == Type],
                 if
                   length(Type_list) == 0 -> 0;
                   true ->
                     Sum = lists:foldl(fun(X, S) -> X#measurement.value + S end, 0, Type_list),
                     Sum / length(Type_list)
                 end;
               error -> {error, "Station don't exists"}
             end
  end.

getDayValues(Type, Date, Monitor) ->
  M = [Measurements || {_,MeasuremenementsLists} <- maps:to_list(Monitor#monitor.data), Measurements <- MeasuremenementsLists ],
  K = [{V#measurement.value, V#measurement.date} || V <- M, V#measurement.type == Type],
  [X || {X,{D,_}} <- K, D == Date].

getDailyMean(Type, Date, Monitor) ->
  M = [Measurements || {_,MeasuremenementsLists} <- maps:to_list(Monitor#monitor.data), Measurements <- MeasuremenementsLists ],
  K = [{V#measurement.value, V#measurement.date} || V <- M, V#measurement.type == Type],
  Data = [X || {X,{D,_}} <- K, D == Date],
  calculateMean(Data).

calculateMean([]) -> 0;
calculateMean(L) -> lists:sum(L) / length(L).


getDailyAverageDataCount(Date, Monitor) ->
  StationsCount = length(maps:keys(Monitor#monitor.data)),
  if
    StationsCount == 0 -> 0;
    true ->
      M = [Measurements#measurement.date || {_, MeasuremenementLists} <- maps:to_list(Monitor#monitor.data), Measurements <- MeasuremenementLists ],
      Data = [D || {D,_} <- M, D == Date],
      length(Data) / StationsCount
  end.




create() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("A",{0,0}, P),
  P2 = pollution:addValue("A",{{2018,05,01},{15,20,12}},"PM10",70,P1),
  P3 = pollution:addValue({0,0},{{2018,05,02},{15,20,12}},"PM10",100,P2),
  P4 = pollution:addValue({0,0},{{2018,05,02},{15,21,12}},"PM2.5",80,P3),
  pollution:addValue({0,0},{{2018,05,02},{16,21,12}},"PM2.5", 1200,P4).

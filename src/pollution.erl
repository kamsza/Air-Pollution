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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getDailyAverageDataCount/2,createStations/0]).

-record(station, {name, location}).
-record(measurement, {date, type, value}).
-record(monitor, {name, location, data}).        % name: name => station
                                                 % location: location => station
                                                 % data: station => measurement


createMonitor() -> #monitor{name = maps:new(), location = maps:new(), data = maps:new()}.

addStation(Name, Location, Monitor) ->
  case maps:is_key(Name, Monitor#monitor.name) or maps:is_key(Location, Monitor#monitor.location) of
    true -> throw("Similar station already exists");
    _ -> #monitor{
      name = (Monitor#monitor.name)#{Name => #station{name = Name, location = Location}},
      location = (Monitor#monitor.location)#{Location => #station{name = Name, location = Location}},
      data = (Monitor#monitor.data)#{#station{name = Name, location = Location} => []}
    }
  end.

addValue(Id, Date, Type, Value, Monitor) ->
  case Id of
    {_,_} -> try maps:find(Id, Monitor#monitor.location) of
               {ok, Station} -> addValue(Station, #measurement{date = Date, type = Type, value = Value}, Monitor)
             catch
               error -> throw("Station don't exists")
             end;
    _ ->     try maps:find(Id, Monitor#monitor.name) of
              {ok, Station} -> addValue(Station, #measurement{date = Date, type = Type, value = Value}, Monitor)
            catch
              error -> throw("Station don't exists")
            end
    end.

addValue(Station, Measurement, Monitor) ->
  {_, List} = maps:find(Station, Monitor#monitor.data),
  case lists:member(Measurement, List) of
    true -> throw("Measurement with given data is already assigned to this station");
    false -> #monitor {
      name = (Monitor#monitor.name),
      location = (Monitor#monitor.location),
      data = (Monitor#monitor.data)#{Station := [Measurement | maps:get(Station, Monitor#monitor.data)]}
    }
  end.

removeValue(Id, Date, Type, Monitor) ->
  case Id of
    {_,_} -> try maps:find(Id, Monitor#monitor.location) of
               {ok, Station} -> removeValueS(Station, Date, Type, Monitor)
             catch
               error -> throw("Station don't exists")
             end;
    _ ->     try maps:find(Id, Monitor#monitor.name) of
               {ok, Station} -> removeValueS(Station, Date, Type, Monitor)
             catch
               error -> throw("Station don't exists")
             end
  end.

removeValueS(Station, Date, Type, Monitor) when is_record(Station, station) ->
  #monitor {
  name = (Monitor#monitor.name),
  location = (Monitor#monitor.location),
  data = (Monitor#monitor.data)#{Station := [X || X <-  maps:get(Station, Monitor#monitor.data), X#measurement.date /= Date andalso X#measurement.type /= Type]}
}.

getOneValue(Id, Date, Type, Monitor) ->
  case Id of
    {_,_} -> try maps:find(Id, Monitor#monitor.location) of
               {ok, Station} ->
                 case lists:search(fun(X) -> X#measurement.date == Date andalso X#measurement.type == Type end, maps:get(Station, Monitor#monitor.data)) of
                   {_, V} -> V#measurement.value;
                   false -> "Value not found"
                 end
             catch error -> throw("Station don't exists") end;
    _ ->     try maps:find(Id, Monitor#monitor.name) of
               {ok, Station} ->
                 case lists:search(fun(X) -> X#measurement.date == Date andalso X#measurement.type == Type end, maps:get(Station, Monitor#monitor.data)) of
                   {_, V} -> V#measurement.value;
                   false -> "Value not found"
                 end
             catch error -> throw("Station don't exists") end
  end.

getStationMean(Id, Type, Monitor) ->
  case Id of
    {_,_} -> try maps:find(Id, Monitor#monitor.location) of
               {ok, Station} ->
                 Type_list = [X || X <- maps:get(Station, Monitor#monitor.data), X#measurement.type == Type],
                 Sum = lists:foldl(fun(X, S) -> X#measurement.value + S end, 0, Type_list),
                 Sum / length(Type_list)
             catch error -> throw("Station don't exists") end;
    _ ->     try maps:find(Id, Monitor#monitor.name) of
               {ok, Station} ->
                 Type_list = [X || X <- maps:get(Station, Monitor#monitor.data), X#measurement.type == Type],
                 Sum = lists:foldl(fun(X, S) -> X#measurement.value + S end, 0, Type_list),
                 Sum / length(Type_list)
             catch error -> throw("Station don't exists")
             end
  end.


getDailyMean(Type, Date, Monitor) ->
  Data = maps:fold(fun(_, V, Acc) -> Acc ++ [X || X <- V, X#measurement.date == Date, X#measurement.type == Type] end, [], Monitor#monitor.data),
  Values = [V || X <- Data, {_,_,_,V} = X],
  Values.


%%  getDailyMean(Type, Date, Monitor) ->
%%  Data = maps:filter(fun(_,V) -> V#measurement.date == Date andalso V#measurement.type == Type end, Monitor#monitor.data),
%%  Sum = maps:fold(fun(_, V, Acc) -> Acc + V#measurement.value end, 0, Data),
%%  Length = maps:fold(fun(_, V, Acc) -> Acc + length(V) end, 0, Data),
%%  Sum / Length.


getDailyAverageDataCount(Date, Monitor) ->
  List = maps:to_list(Monitor#monitor.data),
  Data = lists:foldl(fun(M,Acc) -> lists:merge(Acc,  [X ||  {_, X} = M, X#measurement.date == Date]) end, [], List),
  Data.

%%getDailyAverageDataCount(Date, Monitor) ->
%%  Data = maps:fold(fun(_, V, Acc) -> Acc ++ [X || X <- V, X#measurement.date == Date] end, [], Monitor#monitor.data),
%%  Measurements_am = maps:fold(fun(_, V, Acc) -> Acc + length(V) end, 0, Data),
%%  Station_am = maps:fold(fun(_, _, Acc) -> Acc + 1 end, 0, Data),
%%  Measurements_am / Station_am.


createStations() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("A", {12,12}, P1),
  P3 = pollution:addStation("B", {13, 13},P2),
  P4 = pollution:addStation("C", {14, 14},P3),
  P5 = pollution:addValue("A", calendar:local_time(), "PM2,5", 100, P4),
  P6 = pollution:addValue("A", calendar:local_time(), "PM10", 50, P5),
  P7 = pollution:addValue("B", calendar:local_time(), "PM2,5", 50, P6),
  P8 = pollution:addValue("C", calendar:local_time(), "PM2,5", 20, P7),
  P9 = pollution:addValue("A", calendar:local_time(), "PM2,5", 50, P8).
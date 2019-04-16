%%%-------------------------------------------------------------------
%%% @author kamsz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2019 14:39
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kamsz").

%% API
-export([start/0, stop/0, init/0, loop/1, addStation/2, addValue/4, removeValue/3, getOneValue/3, getDailyMean/3, getStationMean/2, quit/0]).

start() ->
  register(server, spawn(?MODULE, init, [])).

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

stop() ->
  server ! {request, self(), stop}.


loop(Monitor) ->
  receive
    {request, Pid, {addStation, Name, {X, Y}}} ->
      NewMonitor = pollution:addStation(Name, {X,Y}, Monitor),
      loop(NewMonitor),
      Pid ! {reply, ok};
    {request, Pid, {addValue, Id, Date, Type, Value}} ->
      NewMonitor = pollution:addValue(Id, Date, Type, Value, Monitor),
      loop(NewMonitor),
      Pid ! {reply, ok};
    {request, Pid, {removeValue, Id, Date, Type}} ->
      NewMonitor = pollution:addValue(Id, Date, Type, Monitor),
      loop(NewMonitor),
      Pid ! {reply, ok};
    {request, Pid, {getOneValue, Id, Date, Type}} ->
      NewMonitor = pollution:getOneValue(Id, Date, Type, Monitor),
      loop(NewMonitor),
      Pid ! {reply, ok};
    {request, Pid, {getStationMean, Id, Type}} ->
      NewMonitor = pollution:getStationMean(Id, Type, Monitor),
      loop(NewMonitor),
      Pid ! {reply, ok};
    {request, Pid, {getDailyMean, Id, Type, Date}} ->
      NewMonitor = pollution:getDailyMean(Id, Type, Date, Monitor),
      loop(NewMonitor),
      Pid ! {reply, ok};
    {request, Pid, {quit}} ->
      Pid ! {reply, ok}
  end.



%%% CLIENT %%%

call(Message) ->
  server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  after
    10000 -> io:format("Did not received any data")
  end.

addStation(Name, {X, Y}) -> call({addStation, Name, {X,Y}}).
addValue(Id, Date, Type, Value) -> call({addValue, Id, Date, Type, Value}).
removeValue(Id, Date, Type) -> call({removeValue, Id, Date, Type}).
getOneValue(Id, Date, Type) -> call({getOneValue, Id, Date, Type}).
getStationMean(Id, Type) -> call({getStationMean, Id, Type}).
getDailyMean(Id, Type, Date) -> call({getDailyMean, Id, Type, Date}).
quit() -> call({quit}).
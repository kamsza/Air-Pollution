%%%-------------------------------------------------------------------
%%% @author kamsza
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2019 13:56
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kamsza").

%% API
-behaviour(gen_server).
-compile(export_all).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, pollution:createMonitor(),[]).

init(InitialValue) ->
  {ok, InitialValue}.


%% user interface
addStation(Name, {X, Y}) -> gen_server:call(?MODULE, {addStation, Name, {X,Y}}).
addValue(Id, Date, Type, Value) -> gen_server:call(?MODULE, {addValue, Id, Date, Type, Value}).
removeValue(Id, Date, Type) -> gen_server:call(?MODULE, {removeValue, Id, Date, Type}).
getOneValue(Id, Date, Type) -> gen_server:call(?MODULE, {getOneValue, Id, Date, Type}).
getStationMean(Id, Type) -> gen_server:call(?MODULE, {getStationMean, Id, Type}).
getDailyMean(Type, Date) -> gen_server:call(?MODULE, {getDailyMean, Type, Date}).
getDailyAverageDataCount(Date) -> gen_server:call(?MODULE, {getDailyAverageDataCount, Date}).
stop() -> gen_server:call(?MODULE, {stop}).
crash() -> gen_server:cast(?MODULE, {crash}).


%% callbacks
handle_call({addStation, Name, {X,Y}}, _From, Monitor) ->
  case pollution:addStation(Name, {X,Y}, Monitor) of
    {error, Msg} -> {reply, {error, Msg}, Monitor};
    NewMonitor ->   {reply, ok, NewMonitor}
  end;

handle_call({addValue, Id, Date, Type, Value}, _From, Monitor) ->
  case pollution:addValue(Id, Date, Type, Value, Monitor) of
    {error, Msg} -> {reply, {error, Msg}, Monitor};
    NewMonitor ->   {reply, ok, NewMonitor}
  end;

handle_call({removeValue, Id, Date, Type}, _From, Monitor) ->
  case pollution:removeValue(Id, Date, Type, Monitor) of
    {error, Msg} -> {reply, {error, Msg}, Monitor};
    NewMonitor ->   {reply, ok, NewMonitor}
  end;

handle_call({getOneValue, Id, Date, Type}, _From, Monitor) ->
  case pollution:getOneValue(Id, Date, Type, Monitor) of
    {error, Msg} -> {reply, {error, Msg}, Monitor};
    Value ->   {reply, Value, Monitor}
  end;

handle_call({getStationMean, Id, Type}, _From, Monitor) ->
  case pollution:getStationMean(Id, Type, Monitor) of
    {error, Msg} -> {reply, {error, Msg}, Monitor};
    Value ->   {reply, Value, Monitor}
  end;

handle_call({getDailyMean, Type, Date}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Type, Date, Monitor), Monitor};

handle_call({getDailyAverageDataCount, Date}, _From, Monitor) ->
  {reply, pollution:getDailyAverageDataCount(Date, Monitor), Monitor};

handle_call({stop}, _From, _Monitor) ->
  {stop, normal, shutdown_ok, _Monitor}.

terminate(_Reason, _Value) ->
  ok.

handle_cast({crash}, Monitor) ->
  1/0,
  {noreply, Monitor}.


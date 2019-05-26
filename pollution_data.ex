defmodule PollutionData do
  @moduledoc false

  def import(filename \\ "pollution.csv") do
    File.read!(filename) |> String.split("\n")
  end

  def parse(data) do
    Enum.map(data, fn line -> parse_line(line) end)
  end

  def parse_line(line) do
    line = String.replace(line,"\r","")
    list = String.split(line, ",")
    [date, time, x, y, value] = list
    time = time <> ":00"
    date = date |> String.split("-") |> Enum.map(&Integer.parse/1) |> Enum.map(fn {x,_} -> x end) |> Enum.reverse() |> List.to_tuple()
    time = time |> String.split(":") |> Enum.map(&Integer.parse/1) |> Enum.map(fn {x,_} -> x end) |> List.to_tuple()
    {x, _} = Float.parse(x)
    {y, _} = Float.parse(y)
    {value, _} = Integer.parse(value)
    %{:datetime => {date, time}, :location => {x,y}, :pollutionLevel  => value}
  end

  def identify_stations(data) do
    Enum.reduce(data, %{}, fn (m, acc) -> {x, y} = m[:location]
                                          name = "Station  #{x}  #{y}"
                                          Map.put(acc, name, {x, y}) end)
  end

  def add_stations() do
    data = import()
    parsed_data = parse(data)
    stations = identify_stations(parsed_data)
    for {name, location} <- stations do :pollution_server.addStation(name, location) end
  end

  def add_values() do
    data = import()
    parsed_data = parse(data)
    Enum.each(parsed_data, fn m  -> :pollution_server.addValue(m.location, m.datetime, 'PM10', m.pollutionLevel) end)
  end

  def stop_all() do
    :pollution_supervisor.stop()
    IO.puts "Supervisor stopped"

    :pollution_server.stop()
    IO.puts "Server stopped"
  end

  def load_data do
    :pollution_supervisor.start_link()
    IO.puts "Supervisor has started"

    stationsLoadingTime =
      fn -> add_stations() end
      |> :timer.tc
      |> elem(0)
      |> fn t -> t / 1_000_000.0 end.()

    dataLoadingTime =
      fn -> add_values() end
      |> :timer.tc
      |> elem(0)
      |> fn t -> t / 1_000_000.0 end.()

    IO.puts "Stations prepared in #{stationsLoadingTime} s"
    IO.puts "Measurements prepared in #{dataLoadingTime} s"
  end

  def measure_time(function) do
    {time, value} =
      function
      |> :timer.tc


    IO.puts "Time: #{time} ms"
    value
  end

  def measure(function) do
    function
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end

  def run_measurements() do
    r = measure_time(fn -> :pollution_server.getStationMean({20.060, 49.986}, 'PM10') end)
    IO.puts "getStationMean result: #{r} \n"

    r = measure_time(fn -> :pollution_server.getDailyMean('PM10', {2017, 5, 3}) end)
    IO.puts "getDailyMean result: #{r} \n"

    r = measure_time(fn -> :pollution_server.getDailyAverageDataCount({2017, 5, 3}) end)
    IO.puts "getDailyAverageDataCount result: #{r} \n"

    r = measure_time(fn -> :pollution_server.getDailyAverageDataCount({2017, 5, 4}) end)
    IO.puts "getDailyAverageDataCount result: #{r} \n"
  end

end

-module(task1).
-export([start/0, start_conveyor/2, start_truck/2, start_generator/2]).

% Start Package Generator process
start_generator(ConveyorPids, Interval) ->
    spawn(fun() -> generator_loop(ConveyorPids, Interval) end).

% Package generator loop
generator_loop(ConveyorPids, Interval) ->
    Package = {package, erlang:unique_integer([positive])},
    % Distribute the package to a random conveyor belt
    RandomConveyor = lists:nth(rand:uniform(length(ConveyorPids)), ConveyorPids),
    RandomConveyor ! {new_package, Package},
    timer:sleep(Interval),
    generator_loop(ConveyorPids, Interval).

% Start a Conveyor Belt process
start_conveyor(Name, TruckPid) ->
    spawn(fun() -> conveyor_loop(Name, TruckPid) end).

% Conveyor Belt loop
conveyor_loop(Name, TruckPid) ->
    receive
        {new_package, Package} ->
            TruckPid ! {load_package, Package},
            conveyor_loop(Name, TruckPid)
    end.

% Start a Truck process
start_truck(Id, MaxCapacity) ->
    spawn(fun() -> truck_loop(Id, MaxCapacity, MaxCapacity) end).

% Truck loop
truck_loop(Id, Capacity, Remaining) ->
    receive
        {load_package, _Package} ->
            io:format("Truck ~p loaded a package. Remaining space: ~p~n", 
                      [Id, Remaining - 1]),
            if 
                Remaining - 1 == 0 ->
                    io:format("Truck ~p is full. Replacing it...~n", [Id]),
                    truck_loop(Id, Capacity, Capacity);
                true ->
                    truck_loop(Id, Capacity, Remaining - 1)
            end
    end.

start() ->
    TruckPid1 = start_truck(1, 10),
    ConveyorPid1 = start_conveyor(conveyor1, TruckPid1),
    
    TruckPid2 = start_truck(2, 10),
    ConveyorPid2 = start_conveyor(conveyor2, TruckPid2),
    
    % Start package generator
    start_generator([ConveyorPid1, ConveyorPid2], 1000).

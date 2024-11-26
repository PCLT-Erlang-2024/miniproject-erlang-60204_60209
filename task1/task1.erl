-module(task1).
-export([start/0, start_conveyor/2, start_truck/2, start_generator/2]).

% Define ANSI color codes
-define(RESET, "\e[0m").       % Reset all formatting
-define(RED, "\e[31m").        % Red text for Truck messages
-define(GREEN, "\e[32m").      % Green text for Conveyor messages

% Start Package Generator process
start_generator(ConveyorPids, Interval) ->
    spawn(fun() -> generator_loop(ConveyorPids, Interval) end).

% Package generator loop
generator_loop(ConveyorPids, Interval) ->
    Package = erlang:unique_integer([positive]),
    % Distribute the package to a random conveyor belt
    RandomConveyor = lists:nth(rand:uniform(length(ConveyorPids)), ConveyorPids),
    RandomConveyor ! {new_package, Package},
    timer:sleep(Interval),
    generator_loop(ConveyorPids, Interval).

% Start a Conveyor Belt process
start_conveyor(Id, TruckPid) ->
    spawn(fun() -> conveyor_loop(Id, TruckPid, []) end).

% Conveyor Belt loop with buffering
conveyor_loop(Id, TruckPid, Queue) ->
    receive
        {new_package, Package} -> % New package received
            io:format("~sConveyor belt ~p received package with id ~p~s~n", [?GREEN, Id, Package, ?RESET]),
            NewQueue = Queue ++ [Package],
            conveyor_loop(Id, TruckPid, NewQueue);
        {process_queue} -> % Periodically load the truck if a package exists
            case Queue of
                [] -> % Nothing to process
                    conveyor_loop(Id, TruckPid, Queue);
                [Package | Rest] -> % Load a package into the truck
                    io:format("~sConveyor belt ~p loading package ~p into truck ~p~s~n", [?GREEN, Id, Package, Id, ?RESET]),
                    TruckPid ! {load_package, Package},
                    conveyor_loop(Id, TruckPid, Rest)
            end
    after 2000 ->  % Periodically process the queue of packages
        self() ! {process_queue},
        conveyor_loop(Id, TruckPid, Queue)
    end.

% Start a Truck process
start_truck(Id, MaxCapacity) ->
    spawn(fun() -> truck_loop(Id, MaxCapacity, MaxCapacity) end).

% Truck loops
truck_loop(Id, Capacity, 0) -> % Truck Full
    io:format("~sTruck ~p is full. Replacing it...~s~n", [?RED, Id, ?RESET]),
    truck_loop(Id, Capacity, Capacity);

truck_loop(Id, Capacity, Remaining) -> % Truck not full
    receive
        {load_package, Package} ->
            io:format("~sTruck ~p loaded package ~p. Remaining space: ~p~s~n", [?RED, Id, Package, Remaining - 1, ?RESET]),
            truck_loop(Id, Capacity, Remaining - 1)
    end.

start() ->
    TruckPid1 = start_truck(1, 10),
    ConveyorPid1 = start_conveyor(1, TruckPid1),
    
    TruckPid2 = start_truck(2, 10),
    ConveyorPid2 = start_conveyor(2, TruckPid2),

    TruckPid3 = start_truck(3, 10),
    ConveyorPid3 = start_conveyor(3, TruckPid3),
    
    % Start package generator
    start_generator([ConveyorPid1, ConveyorPid2, ConveyorPid3], 1000).

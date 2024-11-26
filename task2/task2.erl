-module(task2).
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
    PackageSize = rand:uniform(3), % Generate a package size between 1 and 5
    Package = {erlang:unique_integer([positive]), PackageSize},
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
        {new_package, {PackageId, PackageSize}} -> % New package received
            io:format("~sConveyor belt ~p: Received package ~p with size ~p~s~n", 
                      [?GREEN, Id, PackageId, PackageSize, ?RESET]),
            NewQueue = Queue ++ [{PackageId, PackageSize}],
            conveyor_loop(Id, TruckPid, NewQueue);
        {process_queue} -> % Periodically load the truck if a package exists
            case Queue of
                [] -> % Nothing to process
                    conveyor_loop(Id, TruckPid, Queue);
                [Package | Rest] -> % Attempt to load a package into the truck
                    io:format("~sConveyor belt ~p: Loading package ~p into truck ~p~s~n", [?GREEN, Id, Package, Id, ?RESET]),
                    TruckPid ! {load_package, Package, self()},
                    conveyor_loop(Id, TruckPid, Rest)
            end;
        {package_rejected, Package} -> % Package rejected by the truck
            io:format("~sConveyor belt ~p: Package ~p with size ~p was rejected and returned to the belt~s~n", 
                      [?GREEN, Id, element(1, Package), element(2, Package), ?RESET]),
            conveyor_loop(Id, TruckPid, [Package | Queue])
    after 2000 ->  % Periodically process the queue of packages
        self() ! {process_queue},
        conveyor_loop(Id, TruckPid, Queue)
    end.

% Start a Truck process
start_truck(Id, MaxCapacity) ->
    spawn(fun() -> truck_loop(Id, MaxCapacity, MaxCapacity) end).

% Truck loops
truck_loop(Id, Capacity, Remaining) ->
    receive
        {load_package, {PackageId, Size}, _} when Size =< Remaining -> % Package fits in the truck
                io:format("~sTruck ~p: Loading package ~p (size: ~p). Remaining capacity: ~p~s~n", 
                            [?RED, Id, PackageId, Size, Remaining - Size, ?RESET]),
                truck_loop(Id, Capacity, Remaining - Size);
        
        {load_package, {PackageId, Size}, ConveyorPid} when Size > Remaining -> % Package too large for the truck
                    io:format("~sTruck ~p rejected package ~p (size: ~p). Not enough capacity.~s~n", 
                              [?RED, Id, PackageId, Size, ?RESET]),
                    ConveyorPid ! {package_rejected, {PackageId, Size}}, % Send package back to the Belt
                    truck_loop(Id, Capacity, Remaining),
                    self() ! {full_truck}; % Replace truck
        {full_truck} -> % Truck is full and needs replacement
            io:format("~sTruck ~p is full. Replacing it...~s~n", [?RED, Id, ?RESET]),
            truck_loop(Id, Capacity, Capacity)
    end.
start() ->
    TruckPid1 = start_truck(1, 10), % Increased capacity for trucks
    ConveyorPid1 = start_conveyor(1, TruckPid1),
    
    TruckPid2 = start_truck(2, 10),
    ConveyorPid2 = start_conveyor(2, TruckPid2),

    TruckPid3 = start_truck(3, 10),
    ConveyorPid3 = start_conveyor(3, TruckPid3),
    
    % Start package generator
    start_generator([ConveyorPid1, ConveyorPid2, ConveyorPid3], 1000).

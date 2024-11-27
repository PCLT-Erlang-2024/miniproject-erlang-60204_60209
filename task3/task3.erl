-module(task3).
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
    spawn(fun() -> conveyor_loop(Id, TruckPid, [], false) end).

% Conveyor Belt loop with buffering
conveyor_loop(Id, TruckPid, Queue, IsPaused) ->
    receive
        {new_package, {PackageId, PackageSize}} when not IsPaused -> % New package received
            io:format("~sConveyor belt ~p: Received package ~p with size ~p~s~n", 
                     [?GREEN, Id, PackageId, PackageSize, ?RESET]),
            NewQueue = Queue ++ [{PackageId, PackageSize}],
            conveyor_loop(Id, TruckPid, NewQueue, IsPaused);
        {process_queue} when not IsPaused -> % Periodically load the truck if a package exists
            case Queue of
                [] -> % Nothing to process
                    conveyor_loop(Id, TruckPid, Queue, IsPaused);
                [Package | Rest] -> % Attempt to load a package into the truck
                    io:format("~sConveyor belt ~p: Loading package ~p into truck ~p~s~n", 
                              [?GREEN, Id, Package, Id, ?RESET]),
                    TruckPid ! {load_package, Package, self()},
                    conveyor_loop(Id, TruckPid, Rest, IsPaused)
            end;
        {package_rejected, Package} when not IsPaused -> % Package rejected by the truck
            io:format("~sConveyor belt ~p: Package ~p with size ~p was rejected and returned to the belt~s~n", 
                      [?GREEN, Id, element(1, Package), element(2, Package), ?RESET]),
            conveyor_loop(Id, TruckPid, [Package | Queue], IsPaused);
        {pause_operation} -> % Pause operation during truck replacement
            io:format("~sConveyor belt ~p: Truck is being replaced. Pausing operation...~s~n", [?GREEN, Id, ?RESET]),
            conveyor_loop(Id, TruckPid, Queue, true);
        {resume_operation} -> % Resume operation after truck replacement
            io:format("~sConveyor belt ~p: Truck is replaced. Resuming operation...~s~n", [?GREEN, Id, ?RESET]),
            conveyor_loop(Id, TruckPid, Queue, false)
    after 2000 ->  % Periodically process the queue of packages
        if not IsPaused ->
            self() ! {process_queue};
        true ->
            ok
        end,
        conveyor_loop(Id, TruckPid, Queue, IsPaused)
    end.

% Start a Truck process
start_truck(Id, MaxCapacity) ->
    spawn(fun() -> truck_loop(Id, MaxCapacity, MaxCapacity) end).

% Truck loops
truck_loop(Id, Capacity, Remaining) ->
    receive
        {load_package, {PackageId, Size}, ConveyorPid} when Size =< Remaining -> % Package fits in the truck
            io:format("~sTruck ~p: Loading package ~p (size: ~p). Remaining capacity: ~p~s~n", 
                      [?RED, Id, PackageId, Size, Remaining - Size, ?RESET]),
            % Check if the truck is now full
            case Remaining - Size of
                0 -> % Truck is full after this package
                    io:format("~sTruck ~p: Is full. Replacing it...~s~n", [?RED, Id, ?RESET]),
                    ConveyorPid ! {pause_operation}, % Notify conveyor to pause
                    RandomDelay = rand:uniform(3) * 1000, % Random delay between 1 and 3 seconds
                    io:format("~sTruck ~p: Is being replaced. Sleeping for ~p milliseconds.~s~n", 
                              [?RED, Id, RandomDelay, ?RESET]),
                    timer:sleep(RandomDelay),
                    ConveyorPid ! {resume_operation}, % Notify conveyor to resume
                    truck_loop(Id, Capacity, Capacity);
                _ -> % Truck still has capacity
                    truck_loop(Id, Capacity, Remaining - Size)
            end;

        {load_package, {PackageId, Size}, ConveyorPid} when Size > Remaining -> % Package too large for the truck
            io:format("~sTruck ~p: Rejected package ~p (size: ~p). Not enough capacity.~s~n", 
                      [?RED, Id, PackageId, Size, ?RESET]),
            ConveyorPid ! {package_rejected, {PackageId, Size}}, % Send package back to the Belt
            io:format("~sTruck ~p: Out of capacity. Replacing it...~s~n", [?RED, Id, ?RESET]),
            ConveyorPid ! {pause_operation}, % Notify conveyor to pause
            RandomDelay = rand:uniform(3) * 1000, % Random delay between 1 and 3 seconds
            io:format("~sTruck ~p: Is being replaced. Sleeping for ~p milliseconds.~s~n", 
                      [?RED, Id, RandomDelay, ?RESET]),
            timer:sleep(RandomDelay),
            ConveyorPid ! {resume_operation}, % Notify conveyor to resume
            truck_loop(Id, Capacity, Capacity)
    end.

% Start the system
start() ->
    TruckPid1 = start_truck(1, 10), % Start Truck 1
    ConveyorPid1 = start_conveyor(1, TruckPid1),

    TruckPid2 = start_truck(2, 10), % Start Truck 2
    ConveyorPid2 = start_conveyor(2, TruckPid2),

    TruckPid3 = start_truck(3, 10), % Start Truck 3
    ConveyorPid3 = start_conveyor(3, TruckPid3),

    % Start package generator
    start_generator([ConveyorPid1, ConveyorPid2, ConveyorPid3], 1000).

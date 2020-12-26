-module(district_manager).
-export([ create/0, put/2, get/2, district_from/5
        , district_name/1, district_ip/1, district_port/1, district_pub_ip/1, district_pub_port/1
        ]).
-record(state, {districts, logs}).
-record(district, {name, ip, port, pub_ip, pub_port}).

% district_from(Ip, Port, PubIp, PubPort) -> District
district_from(Name, Ip, Port, PubIp, PubPort) ->
    #district{name = Name, ip = Ip, port = Port, pub_ip = PubIp, pub_port = PubPort}.

district_name(District) ->
    District#district.name.

district_ip(District) ->
    District#district.ip.

district_port(District) ->
    District#district.port.

district_pub_ip(District) ->
    District#district.pub_ip.

district_pub_port(District) ->
    District#district.pub_port.

% create() -> Pid
create() ->
    spawn(fun() -> district_manager(#state{ districts = #{} 
                                          , logs = []
                                          }) end).

% add_log(State, String) -> State
add_log(State, Message) ->
    io:fwrite("Log ~w:" ++ Message ++ "~n", [self()]),
    State#state{logs = [Message] ++ State#state.logs }.

district_manager(State) ->
    receive
        {{put_district, Pid}, {District}} ->
            {Result, NewState} = district_manager_put(State, District),
            Pid ! {Result, self()},
            district_manager(NewState);
        {{get_district, Pid}, {Name}} ->
            District = maps:get(Name, State#state.districts, unregistered),
            Pid ! {District, self()},
            district_manager(State);
        _ ->
            error_logger:error_report("Invalid message received."),
            district_manager(State)
    end.

% district_manager_put(State, District) -> {ok, State}
district_manager_put(State, District) ->
    NewDistricts = maps:put(District#district.name, District, State#state.districts),
    {ok, add_log( State#state{districts = NewDistricts}
                , io_lib:format("Added district ~s", [ District#district.name ]))}.
    
    

% put(DistrictManager, District) -> ok
put(Manager, District) ->
    Manager ! {{put_district, self()}, {District}},
    receive
        {R, Manager} ->
            R
    end.

% get(Manager, Name) -> unregistered | District
get(Manager, Name) ->
    Manager ! {{get_district, self()}, {Name}},
    receive
        {R, Manager} ->
            R
    end.


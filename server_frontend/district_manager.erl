-module(district_manager).
-export([ create/0, put/2, get/2, district_from/6, send_await/3
        , district_name/1, district_ip/1, district_port/1, district_pub_ip/1, district_pub_port/1
        ]).
-record(state, {districts, logs}).
-record(district, {name, ip, port, pub_ip, pub_port, sock}).

% district_from(Ip, Port, PubIp, PubPort, Sock) -> District
district_from(Name, Ip, Port, PubIp, PubPort, Sock) ->
    #district{name = Name, ip = Ip, port = Port, pub_ip = PubIp, pub_port = PubPort, sock = Sock}.

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
            %io:fwrite("~p\n", [NewState#state.districts]),
            district_manager(NewState);
        {{get_district, Pid}, {Name}} ->
            %io:fwrite("~p\n", [State#state.districts]),
            District = maps:get(Name, State#state.districts, unregistered),
            Pid ! {District, self()},
            district_manager(State);
        {{send_request, Pid}, {Name, Request}} ->
            case maps:get(Name, State#state.districts, unregistered) of
                unregistered ->
                    Pid ! {fail, self()};
                District ->
                    Socket = District#district.sock,
                    gen_tcp:send(Socket, Request),
                    receive
                        {tcp, Socket, Data} ->
                            Pid ! {Data, self()}
                    end
            end,
            district_manager(State);
        _ ->
            district_manager(State)
    end.

% district_manager_put(State, District) -> {ok, State} | {fail, State}
district_manager_put(State, District) ->
    IsKey = maps:is_key(District#district.name, State#state.districts),
    if 
        IsKey ->
            {fail, State};
        true ->
            NewDistricts = maps:put(District#district.name, District, State#state.districts),
            {ok, add_log( State#state{districts = NewDistricts}
                        , io_lib:format("Added district ~s", [ District#district.name ]))}
    end.
    
    

% put(DistrictManager, District) -> ok | fail
put(Manager, District) ->
    Manager ! {{put_district, self()}, {District}},
    receive
        {fail, Manager} ->
            fail;
        {R, Manager} ->
            gen_tcp:controlling_process(District#district.sock, Manager),
            R
    end.

% get(Manager, Name) -> unregistered | District
get(Manager, Name) ->
    Manager ! {{get_district, self()}, {Name}},
    receive
        {R, Manager} ->
            R
    end.

% send(Manager, Name) -> fail | Response
send_await(Manager, Name, Request) ->
    Manager ! {{send_request, self()}, {Name, Request}},
    receive
        {R, Manager} ->
            R
    end.

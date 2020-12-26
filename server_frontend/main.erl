-module(main).
-export([start_server/0, start_server/1, decode_request/1]).
-import(mochijson, [decode/1]).
-record(request, {map = #{}}).
-record(cserver_state, {distManager, authManager, sock, logged = false}).

% decode_request(#request, Key-Value Pairs) -> #request
request_from_decoded(Acc, []) ->
    Acc;
request_from_decoded(Acc, [{Key, Value} | Kvs]) ->
    NewAcc = Acc#request{map = maps:put(Key, Value, Acc#request.map)},
    request_from_decoded(NewAcc, Kvs).

% decode_request(Bytes) -> #request | no_parse
decode_request(Request) ->
    try mochijson:decode(Request) of
        {struct, Decoded} ->
            request_from_decoded(#request{}, Decoded);
        _ -> no_parse
    catch
        _:_ ->
            no_parse
    end.

% start_server() -> {ok, {Pid, Pid}}
start_server() ->
    start_server(10000).

% start_server(Integer) -> {ok, Pid}
start_server(Port) ->
  {ok, SSock} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]),
  
  AuthManager = auth_manager:create(),
  DistManager = district_manager:create(),
  MainLoop = spawn(fun() -> main_loop({DistManager, AuthManager, SSock}) end),
  {ok, {MainLoop}}.

main_loop({DistManager, AuthManager, SSock}) ->
    {ok, Sock} = gen_tcp:accept(SSock),
    io:format("accepted connection.~n"),
    Pid = spawn(fun() -> serve_connection(#cserver_state
                                      { distManager = DistManager
                                      , authManager = AuthManager
                                      , sock = Sock}) end),
    gen_tcp:controlling_process(Sock, Pid),
    main_loop({DistManager, AuthManager, SSock}).

serve_connection(State) ->
    receive
        {tcp, _, Data} ->
            case serve_connection_data(State, Data) of
                {updateState, NewState} ->
                    io:fwrite("newState: ~p~n", [NewState]),
                    serve_connection(NewState);
                {ended} ->
                    io:format("Closed.~n");
                F ->
                    io:fwrite("~p~n", [F]),
                    serve_connection(State)
            end;
        {tcp_closed, _} ->
            io:format("Closed.~n");
        _ ->
            io:format("invalid message received~n"),
            serve_connection(State)
    end.

% serve_client_request_registration(State, Request) -> {updateState, State} | _
serve_connection_data(State, Data) ->
    case decode_request(Data) of
        no_parse ->
            io:format("No parse.~n");
        Request ->
            io:format("~p~n", [Request]),
            serve_connection_request(State, Request)
    end.

% serve_client_request_registration(State, Request) -> {updateState, State} | _
serve_connection_request(State, Request) ->
    try
        case maps:get("RequestType", Request#request.map, unknown) of
            "Registration" ->
                serve_client_request_registration(State, Request);
            "Authentication" ->
                serve_client_request_authentication(State, Request);
            "NotifyLocation" ->
                serve_client_request_notifyLocation(State, Request);
            "ProbeLocation" ->
                serve_client_request_probeLocation(State, Request);
            "Logout" ->
                gen_tcp:send(State#cserver_state.sock, make_response("Logout", 200)),
                {updateState, State#cserver_state{logged = false}};
            _ ->
                case maps:get("request_type", Request#request.map, unknown) of
                    "AnnounceDistrictServer" ->
                        serve_server_request_announce(State, Request);
                    _ ->
                        gen_tcp:send(State#cserver_state.sock, make_response(400)),
                        io:format("Unrecognized request.")
                end
        end
    catch
        error:{badkey,_} ->
            gen_tcp:send(State#cserver_state.sock, make_response("Registration", 400))
    end.

% serve_client_request_registration(State, Request) -> {updateState, State} | _
serve_client_request_registration(State, Request) ->
    Account = auth_manager:account_from
                ( maps:get("Name", Request#request.map)
                , maps:get("Password", Request#request.map)
                , maps:get("Domicile", Request#request.map)),
    case auth_manager:put_account(State#cserver_state.authManager, sync, Account) of
        ok ->
            gen_tcp:send(State#cserver_state.sock, make_response("Registration", 201));
        _ ->
            gen_tcp:send(State#cserver_state.sock, make_response("Registration", 403))
    end.

% serve_client_request_authentication(State, Request) -> {updateState, State} | _
serve_client_request_authentication(State, Request) ->
    case auth_manager:auth_account( State#cserver_state.authManager
                                  , maps:get("Name", Request#request.map)
                                  , maps:get("Password", Request#request.map)) of
        ok ->
            gen_tcp:send(State#cserver_state.sock, make_response("Authentication", 200)),
            {updateState, State#cserver_state{logged = maps:get("Name", Request#request.map)}};
        _ ->
            gen_tcp:send(State#cserver_state.sock, make_response("Authentication", 403))
    end.

% serve_client_request_authentication(State, Request) -> {updateState, State} | _
serve_client_request_notifyLocation(State, Request) ->
    case State#cserver_state.logged of
        false ->
            gen_tcp:send(State#cserver_state.sock, make_response("NotifyLocation", 401));
        _Client ->
            _ = maps:get("Latitude", Request#request.map),
            _ = maps:get("Longitude", Request#request.map),
            gen_tcp:send(State#cserver_state.sock, make_response("NotifyLocation", 404, "Awaiting implementation."))
    end.

% serve_client_request_probeLocation(State, Request) -> {updateState, State} | _
serve_client_request_probeLocation(State, Request) ->
    _ = maps:get("Latitude", Request#request.map),
    _ = maps:get("Longitude", Request#request.map),
    gen_tcp:send(State#cserver_state.sock, make_response("ProbeLocation", 404, "Awaiting implementation.")).

serve_server_request_announce(State, Request) ->
    Name = maps:get("districtName", Request#request.map, badkey),
    Ip = maps:get("server_ip", Request#request.map, badkey),
    Port = maps:get("server_port", Request#request.map, badkey),
    PubIp = maps:get("pub_notifications_ip", Request#request.map, badkey),
    PubPort = maps:get("pub_notifications_port", Request#request.map, badkey),
    case lists:any(fun(X) -> X == badkey end, [Name, Ip, Port, PubIp, PubPort]) of
        false ->
            District = district_manager:district_from(Name, Ip, Port, PubIp, PubPort),
            district_manager:put(State#cserver_state.distManager, District),
            gen_tcp:send(State#cserver_state.sock, make_response("AnnounceDistrictServer", 200));
        _ ->
            gen_tcp:send(State#cserver_state.sock, make_response("AnnounceDistrictServer", 400))
    end.

make_response(Code) ->
    list_to_binary(io_lib:format( "{\"version\": \"1.0.0\", \"code\": \"~p\"}", [Code])).

make_response(ReqType, Code) ->
    S = io_lib:format( "{\"version\": \"1.0.0\", \"RequestType\": ~p, \"code\": \"~p\"}"
                     , [ReqType, Code]),
    list_to_binary(S).

make_response(ReqType, Code, Body) ->
    S = io_lib:format( "{\"version\": \"1.0.0\", \"RequestType\": ~p, \"code\": \"~p\", \"body\": ~p}"
                     , [ReqType, Code, Body]),
    list_to_binary(S).

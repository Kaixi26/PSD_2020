-module(main).
-export([start_server/0, start_server/1, decode_request/1, intercalate/2]).
-import(mochijson, [decode/1, encode/1]).
-record(request, {map = #{}}).
-record(state, {distManager, authManager, sock, logged = false}).

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
    Pid = spawn(fun() -> serve_connection(#state
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
                    ok;
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
            "NotifyInfection" ->
                serve_client_request_notifyInfection(State, Request);
            "NotifyLocation" ->
                serve_client_request_notifyLocation(State, Request);
            "ProbeLocation" ->
                serve_client_request_probeLocation(State, Request);
            "Logout" ->
                gen_tcp:send(State#state.sock, make_response("Logout", 200)),
                {updateState, State#state{logged = false}};
            "Subscribe" ->
                serve_client_request_subscribe(State, Request);
            "Unsubscribe" ->
                serve_client_request_unsubscribe(State, Request);
            "GetSubscriptions" ->
                serve_client_request_getSubscriptions(State, Request);
            "AnnounceDistrictServer" ->
                serve_server_request_announce(State, Request);
            _ ->
                gen_tcp:send(State#state.sock, make_response(400)),
                io:format("Unrecognized request.")
        end
    catch
        error:{badkey,_} ->
            gen_tcp:send(State#state.sock, make_response("Registration", 400))
    end.

% serve_client_request_registration(State, Request) -> {updateState, State} | _
serve_client_request_registration(State, Request) ->
    Account = auth_manager:account_from
                ( maps:get("Name", Request#request.map)
                , maps:get("Password", Request#request.map)
                , maps:get("Domicile", Request#request.map)),
    case auth_manager:put_account(State#state.authManager, sync, Account) of
        ok ->
            gen_tcp:send(State#state.sock, make_response("Registration", 201));
        _ ->
            gen_tcp:send(State#state.sock, make_response("Registration", 403))
    end.

% serve_client_request_authentication(State, Request) -> {updateState, State} | _
serve_client_request_authentication(State, Request) ->
    case auth_manager:auth_account( State#state.authManager
                                  , maps:get("Name", Request#request.map)
                                  , maps:get("Password", Request#request.map)) of
        ok ->
            gen_tcp:send(State#state.sock, make_response("Authentication", 200)),
            {updateState, State#state{logged = maps:get("Name", Request#request.map)}};
        _ ->
            gen_tcp:send(State#state.sock, make_response("Authentication", 403))
    end.

% serve_client_request_authentication(State, Request) -> {updateState, State} | _
serve_client_request_notifyLocation(State, Request) ->
    case State#state.logged of
        false ->
            gen_tcp:send(State#state.sock, make_response("NotifyLocation", 401));
        ClientName ->
            Lat  = maps:get("Latitude", Request#request.map, false),
            Long = maps:get("Longitude", Request#request.map, false),
            NoFalse = lists:all(fun(X) -> X =/= false end, [Lat, Long]),
            if
                 NoFalse ->
                    S = io_lib:format( "{\"username\":\"~p\",\"location\":{\"latitude\":~p,\"longitude\":~p},\"request_type\":\"NotifyLocation\",\"version\":\"1.0.0\"}"
                                     , [ClientName, Lat, Long]),
                    District = auth_manager:account_district(auth_manager:get_account(State#state.authManager, ClientName)),
                    case district_manager:send_await(State#state.distManager, District, list_to_binary(S)) of
                        fail ->
                    gen_tcp:send(State#state.sock, make_response("NotifyLocation", 404));
                        Response ->
                            gen_tcp:send(State#state.sock, Response)
                    end;
                true ->
                    gen_tcp:send(State#state.sock, make_response("NotifyLocation", 404))
            end
    end.

% serve_client_request_probeLocation(State, Request) -> _
serve_client_request_probeLocation(State, Request) ->
    case State#state.logged of
        false ->
            gen_tcp:send(State#state.sock, make_response("ProbeLocation", 401));
        Username ->
            Lat = maps:get("Latitude", Request#request.map, badkey),
            Long = maps:get("Longitude", Request#request.map, badkey),
            case lists:any(fun(X) -> X == badkey end, [Lat, Long]) of
                true ->
                    gen_tcp:send(State#state.sock, make_response("ProbeLocation", 404));
                _ ->
                    S = io_lib:format('{"location":{"latitude":~p,"longitude":~p},"RequestType":"ProbeLocation","version":"1.0.0"}'
                                     , [Lat, Long]),
                    District = auth_manager:account_district(auth_manager:get_account(State#state.authManager, Username)),
                    case district_manager:send_await(State#state.distManager, District, list_to_binary(S)) of
                        fail ->
                    gen_tcp:send(State#state.sock, make_response("NotifyLocation", 404));
                        Response ->
                            gen_tcp:send(State#state.sock, Response)
                    end
            end
    end.

serve_client_request_notifyInfection(State, _Request) ->
    case State#state.logged of
        false ->
            gen_tcp:send(State#state.sock, make_response("NotifyInfection", 401));
        Username ->
            S = io_lib:format('{"username":~p,"RequestType":"NotifyInfection","version":"1.0.0"}'
                             , [Username]),
            District = auth_manager:account_district(auth_manager:get_account(State#state.authManager, Username)),
            case district_manager:send_await(State#state.distManager, District, list_to_binary(S)) of
                fail ->
                    gen_tcp:send(State#state.sock, make_response("NotifyInfection", 404));
                Response ->
                    gen_tcp:send(State#state.sock, Response)
            end
    end.

% serve_client_request_subscription(State, Request) -> _
serve_client_request_subscribe(State, Request) ->
    case State#state.logged of
        false ->
            gen_tcp:send(State#state.sock, make_response("Subscribe", 401));
        Username ->
            case maps:get("district", Request#request.map, badkey) of
                badkey ->
                    gen_tcp:send(State#state.sock, make_response("Subscribe", 404));
                District ->
                    case auth_manager:add_sub(State#state.authManager, Username, District) of
                        ok ->
                            gen_tcp:send(State#state.sock, make_response("Subscribe", 201));
                        _ ->
                            gen_tcp:send(State#state.sock, make_response("Subscribe", 403))
                    end
            end
    end.

serve_client_request_unsubscribe(State, Request) ->
    case State#state.logged of
        false ->
            gen_tcp:send(State#state.sock, make_response("Unsubscribe", 401));
        Username ->
            case maps:get("district", Request#request.map, badkey) of
                badkey ->
                    gen_tcp:send(State#state.sock, make_response("Unsubscribe", 404));
                District ->
                    case auth_manager:rm_sub(State#state.authManager, Username, District) of
                        ok ->
                            gen_tcp:send(State#state.sock, make_response("Unsubscribe", 201));
                        _ ->
                            gen_tcp:send(State#state.sock, make_response("Unsubscribe", 403))
                    end
            end
    end.

serve_client_request_getSubscriptions(State, _Request) ->
    case State#state.logged of
        false ->
            gen_tcp:send(State#state.sock, make_response("GetSubscriptions", 401));
        Username ->
            Subs = auth_manager:account_subscriptions(
                     auth_manager:get_account(State#state.authManager, Username)),
            IPs = lists:map(
                    fun(D) ->
                        case district_manager:get(State#state.distManager, D) of
                            unregistered -> unregistered;
                            Dist -> { D
                                    , district_manager:district_pub_ip(Dist)
                                    , district_manager:district_pub_port(Dist)}
                        end
                    end, Subs),
            FilteredIPs = lists:filter(fun(X) -> X =/= unregistered end, IPs),
            StrIPs = intercalate(",", lists:map(
                       fun({D, IP, Port}) ->
                               io_lib:format('~p:{"ip":~p,"port":~p}' , [D, IP, Port])
                       end, FilteredIPs)),
            Response = list_to_binary(io_lib:format( '{"version": "1.0.0", "ReplyType": "GetSubscriptions", "servers":{~s}}'
                                                   , [StrIPs])),
            gen_tcp:send(State#state.sock, Response)
    end.

serve_server_request_announce(State, Request) ->
    Name = maps:get("districtName", Request#request.map, badkey),
    Ip = maps:get("server_ip", Request#request.map, badkey),
    Port = maps:get("server_port", Request#request.map, badkey),
    PubIp = maps:get("pub_notifications_ip", Request#request.map, badkey),
    PubPort = maps:get("pub_notifications_port", Request#request.map, badkey),
    case lists:any(fun(X) -> X == badkey end, [Name, Ip, Port, PubIp, PubPort]) of
        false ->
            District = district_manager:district_from(Name, Ip, Port, PubIp, PubPort, State#state.sock),
            case district_manager:put(State#state.distManager, District) of
                ok ->
                    gen_tcp:send(State#state.sock, make_response("AnnounceDistrictServer", 201));
                fail ->
                    gen_tcp:send(State#state.sock, make_response("AnnounceDistrictServer", 403))
            end,
            {ended};
        _ ->
            gen_tcp:send(State#state.sock, make_response("AnnounceDistrictServer", 400))
    end.

make_response(Code) ->
    list_to_binary(io_lib:format( "{\"version\": \"1.0.0\", \"code\": \"~p\"}", [Code])).

make_response(ReqType, Code) ->
    S = io_lib:format( "{\"version\": \"1.0.0\", \"ReplyType\": ~p, \"code\": \"~p\"}"
                     , [ReqType, Code]),
    list_to_binary(S).

% make_request(ReqType, Extras) ->
%     S = io_lib:format( "{\"version\": \"1.0.0\", \"RequestType\": ~p, ~p}"
%                      , [ReqType, Extras]),
%     list_to_binary(S).

intercalate(_, []) ->
    [];
intercalate(_, [L]) ->
    L;
intercalate(X, [L|Ls]) ->
    [L] ++ X ++ intercalate(X, Ls).

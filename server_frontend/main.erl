-module(main).
-export([start_server/0, start_server/1, decode_request/1]).
-import(mochijson, [decode/1]).
-import(auth_manager, [create/0, account_from/3, put_account/3, auth_account/3]).
-record(request, {map = #{}}).
-record(state, {auth_manager, logs = []}).

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
  Manager = spawn(fun() -> manager(#state{auth_manager = AuthManager}) end),
  MainLoop = spawn(fun() -> main_loop({Manager, AuthManager, SSock}) end),
  {ok, {Manager, MainLoop}}.

manager(State) ->
    manager(State).

main_loop({Manager, AuthManager, SSock}) ->
    {ok, Sock} = gen_tcp:accept(SSock),
    io:format("accepted connection.~n"),
    Pid = spawn(fun() -> serve_client({Manager, AuthManager, Sock}) end),
    gen_tcp:controlling_process(Sock, Pid),
    main_loop({Manager, AuthManager, SSock}).

serve_client({Manager, AuthManager, Sock}) ->
    receive
        {tcp, _, Data} ->
            case decode_request(Data) of
                no_parse ->
                    io:format("No parse.~n");
                Request ->
                    io:format("~p~n", [Request]),
                    serve_client_request({AuthManager, Sock}, Request)
            end;
        {tcp_closed, _} ->
            io:format("Closed.~n");
        _ ->
            io:format("invalid message received~n")
    end,
    serve_client({Manager, AuthManager, Sock}).

serve_client_request({AuthManager, Sock}, Request) ->
    try
        case maps:get("Request_Type", Request#request.map, unknown) of
            "Registration" ->
                serve_client_request_registration({AuthManager, Sock}, Request);
            "Authentication" ->
                serve_client_request_authentication({AuthManager, Sock}, Request);
            _ ->
                gen_tcp:send(Sock, make_response(400)),
                io:format("Unrecognized request.")
        end
    catch
        error:{badkey,_} ->
            gen_tcp:send(Sock, make_response("Registration", 400))
    end.

serve_client_request_registration({AuthManager, Sock}, Request) ->
    Account = auth_manager:account_from
                ( maps:get("Name", Request#request.map)
                , maps:get("Password", Request#request.map)
                , maps:get("Domicile", Request#request.map)),
    case auth_manager:put_account(AuthManager, sync, Account) of
        ok ->
            gen_tcp:send(Sock, make_response("Registration", 201));
        _ ->
            gen_tcp:send(Sock, make_response("Registration", 403))
    end.

serve_client_request_authentication({AuthManager, Sock}, Request) ->
    case auth_account(AuthManager
                     , maps:get("Name", Request#request.map)
                     , maps:get("Password", Request#request.map)) of
        ok ->
            gen_tcp:send(Sock, make_response("Authentication", 200));
        _ ->
            gen_tcp:send(Sock, make_response("Authentication", 403))
    end.

make_response(Code) ->
    list_to_binary(io_lib:format( "{\"version\": \"1.0.0\", \"code\": \"~p\"}", [Code])).

make_response(ReqType, Code) ->
    S = io_lib:format( "{\"version\": \"1.0.0\", \"Request_Type\": ~p, \"code\": \"~p\"}"
                     , [ReqType, Code]),
    list_to_binary(S).

make_response(ReqType, Code, Body) ->
    S = io_lib:format( "{\"version\": \"1.0.0\", \"Request_Type\": ~p, \"code\": \"~p\", \"body\": ~p}"
                     , [ReqType, Code, Body]),
    list_to_binary(S).

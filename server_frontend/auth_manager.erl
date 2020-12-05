-module(auth_manager).
-export([create/0, account_from/3, put_account/3, auth_account/3]).
-record(state, {accounts, logs}).
-record(account, {name, pass, res}).

% account_from(Username, Password) -> Account
account_from(Username, Password, Residency) ->
    #account{name = Username, pass = Password, res = Residency}.

% create() -> Pid
create() ->
    spawn(fun() -> auth_manager(#state{ accounts = #{} 
                                      , logs = []
                                      }) end).

% add_account(State, Account) -> State | already_exists
add_account(State, Account) ->
    case maps:get(Account#account.name, State#state.accounts, unregistered) of
        unregistered ->
            NewAccounts = maps:put(Account#account.name, Account, State#state.accounts),
            {ok, add_log( State#state{accounts = NewAccounts}
                   , io_lib:format("Added Account ~s ~s ~s", [ Account#account.name
                                                             , Account#account.pass
                                                             , Account#account.res]))};
        _ ->
            {already_exists, add_log( State
                    , io_lib:format("Tried to add ~s but username already exists", [Account#account.name]))}
    end.

% add_log(State, String) -> State
add_log(State, Message) ->
    io:fwrite("Log ~w:" ++ Message ++ "~n", [self()]),
    State#state{logs = [Message] ++ State#state.logs }.

auth_manager(State) ->
    receive
        {{put_account, Pid}, {Account}} ->
            auth_manager_put_account(State, Pid, Account);
        {{auth_account, Pid}, {Username, Password}} ->
            auth_manager_auth_account(State, Pid, Username, Password);
        _ ->
            error_logger:error_report("Invalid message received."),
            auth_manager(State)
    end.

auth_manager_put_account(State, Pid, Account) ->
    case add_account(State, Account) of
        {already_exists, NewState} ->
            Pid ! {already_exists, self()},
            auth_manager(NewState);
        {ok, NewState} ->
            Pid ! {ok, self()},
            auth_manager(NewState)
    end.

auth_manager_auth_account(State, Pid, Username, Password) ->
    case maps:get(Username, State#state.accounts, unregistered) of
        unregistered ->
            Pid ! {unregistered, self()};
        #account{name = Username, pass = Password} ->
            Pid ! {ok, self()};
        _ ->
            Pid ! {invalid_pass, self()}
    end,
    auth_manager(State).

% Mode = sync | async
% put_account -> ok | invalid_format | already_exists
put_account(Manager, Mode, #account{name = Name, pass = Pass, res = Res}) ->
    Account = #account{name = Name, pass = Pass, res = Res},
    Manager ! {{put_account, self()}, {Account}},
    case Mode of
        sync ->
            receive
                {ok, Manager} ->
                    ok;
                {R, Manager} ->
                    R
            end;
        async ->
            ok
    end;
put_account(_, _, _) ->
    invalid_format.

% auth_account(Pid, String) -> ok | unregistered | invalid_pass
auth_account(Manager, Username, Password) ->
    Manager ! {{auth_account, self()}, {Username, Password}},
    receive
        {R, Manager} ->
            R
    end.

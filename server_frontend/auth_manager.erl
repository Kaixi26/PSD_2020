-module(auth_manager).
-export([create/0, account_from/3, put_account/3, auth_account/3, account_name/1
        , account_district/1, get_account/2, add_sub/3, rm_sub/3
        , account_subscriptions/1
        ]).
-record(state, {accounts, logs}).
-record(account, {name, pass, res, sub}).

% account_from(Username, Password) -> Account
account_from(Username, Password, Residency) ->
    #account{name = Username, pass = Password, res = Residency, sub = []}.

account_name(Account) ->
    Account#account.name.

account_district(Account) ->
    Account#account.res.

account_subscriptions(Account) ->
    Account#account.sub.

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
    % io:fwrite("Log ~w:" ++ Message ++ "~n", [self()]),
    State#state{logs = [Message] ++ State#state.logs }.

auth_manager(State) ->
    receive
        {{put_account, Pid}, {Account}} ->
            auth_manager_put_account(State, Pid, Account);
        {{auth_account, Pid}, {Username, Password}} ->
            auth_manager_auth_account(State, Pid, Username, Password);
        {{get_account, Pid}, {Username}} ->
            Pid ! {maps:get(Username, State#state.accounts, unregistered), self()},
            auth_manager(State);
        {{add_sub, Pid}, {Username, District}} ->
            auth_manager_add_sub(State, Pid, Username, District);
        {{rm_sub, Pid}, {Username, District}} ->
            auth_manager_rm_sub(State, Pid, Username, District);
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

auth_manager_add_sub(State, Pid, Username, District) ->
    case maps:get(Username, State#state.accounts, unregistered) of
        unregistered ->
            Pid ! {unregistered, self()},
            auth_manager(State);
        Account ->
            if 
                length(Account#account.sub) < 3 ->
                    NewAccount = Account#account{sub = Account#account.sub ++ [District]},
                    NewState = State#state{accounts = maps:put(Username, NewAccount, State#state.accounts)},
                    Pid ! {ok, self()},
                    auth_manager(NewState);
                true ->
                    Pid ! {exceed, self()},
                    auth_manager(State)
            end
    end.

auth_manager_rm_sub(State, Pid, Username, District) ->
    case maps:get(Username, State#state.accounts, unregistered) of
        unregistered_ ->
            Pid ! {unregistered, self()},
            auth_manager(State);
        Account ->
            NewAccount = Account#account{sub = Account#account.sub -- [District]},
            NewState = State#state{accounts = maps:put(Username, NewAccount, State#state.accounts)},
            Pid ! {ok, self()},
            auth_manager(NewState)
    end.

% Mode = sync | async
% put_account -> ok | invalid_format | already_exists
put_account(Manager, Mode, Account) ->
    case Account of
        #account{} ->
            Manager ! {{put_account, self()}, {Account}},
            case Mode of
                sync ->
                    receive {R, Manager} -> R end;
                async ->
                    ok
            end;
         _ ->
            invalid_format
    end.

% auth_account(Pid, String) -> ok | unregistered | invalid_pass
auth_account(Manager, Username, Password) ->
    Manager ! {{auth_account, self()}, {Username, Password}},
    receive {R, Manager} -> R end.

% auth_account(Pid, String) -> Account | unregistered
get_account(Manager, Username) ->
    Manager ! {{get_account, self()}, {Username}},
    receive {R, Manager} -> R end.

% auth_account(Pid, String) -> ok | exceeded | unregistered
add_sub(Manager, Username, District) ->
    Manager ! {{add_sub, self()}, {Username, District}},
    receive {R, Manager} -> R end.

% rm_sub(Manager, Username, District) -> ok | unregistered
rm_sub(Manager, Username, District) ->
    Manager ! {{rm_sub, self()}, {Username, District}},
    receive {R, Manager} -> R end.

-module(client_manager).
-export([create/0, add_logged/2, rm_logged/2, add_contact/2]).
-record(state, {clients, had_contact}).

% create() -> Pid
create() ->
    spawn(fun() -> client_manager(#state{ clients = #{}
                                        , had_contact = sets:new()
                                  }) end).

client_manager(State) ->
    receive
        {{add_logged, Pid}, {Username}} ->
            io:fwrite("Client manager, adding (~s, ~w)~n", [Username, Pid]),
            LoggedAs = maps:get(Username, State#state.clients, []),
            NewClients = maps:put(Username, LoggedAs++[Pid], State#state.clients),
            self() ! {check_contact, Username},
            client_manager(State#state{ clients = NewClients});

        {{rm_logged, Pid}, {Username}} ->
            io:fwrite("Client manager, removing (~s, ~w)~n", [Username, Pid]),
            LoggedAs = maps:get(Username, State#state.clients, []),
            NewClients = maps:put(Username, LoggedAs--[Pid], State#state.clients),
            client_manager(State#state{ clients = NewClients});

        {add_contact, Username} ->
            io:fwrite("Client manager, adding notification for ~s~n", [Username]),
            NewContacts = sets:add_element(Username, State#state.had_contact),
            self() ! {check_contact, Username},
            client_manager(State#state{ had_contact = NewContacts });

        {check_contact, Username} ->
            io:fwrite("Client manager, checking contact for ~s~n", [Username]),
            case { sets:is_element(Username, State#state.had_contact)
                 , maps:get(Username, State#state.clients, [])} of
                {false, _} ->
                    client_manager(State);
                {_, []} ->
                    client_manager(State);
                {_, LoggedClients} ->
                    lists:map(fun(Pid) -> Pid ! {warn_contact} end, LoggedClients),
                    NewContacts = sets:del_element(Username, State#state.had_contact),
                    client_manager(State#state{ had_contact = NewContacts })
            end;

        _ ->
            error_logger:error_report("Invalid message received."),
            client_manager(State)
    end.
    

add_logged(Manager, Username) ->
    Manager ! {{add_logged, self()}, {Username}}.

rm_logged(Manager, Username) ->
    Manager ! {{rm_logged, self()}, {Username}}.

add_contact(Manager, Username) ->
    Manager ! {add_contact, Username}.

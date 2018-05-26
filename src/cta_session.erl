-module(cta_session).

-include("ct_auth.hrl").

-export([new/3,
         close/1,
         close_all_of_realm/1,

         set_auth_details/4,
         authenticate/2,
         add_subscription/2,
         has_subscription/2,
         remove_subscription/2,

         add_registration/2,
         has_registration/2,
         remove_registration/2,

         get_peer/1,
         get_id/1,
         get_realm/1,
         get_subscriptions/1,
         get_registrations/1,
         to_map/1,

         is_authenticated/1,
         get_authid/1,
         get_authrole/1,

         lookup/1,
         lookup_by_id/1,
         lookup_by_realm/1,


         init/0
        ]).


init() ->
    create_table().

lookup(SessionId) ->
    lookup_by_id(SessionId).

to_map(#cta_session{
          id = SessionId,
          realm = Realm,
          authid = AuthId,
          authrole = AuthRole,
          authprovider = AuthProvider,
          authmethod = AuthMethod,
          authenticated = Authenticated,
          subscriptions = Subs,
          registrations = Regs,
          peer_at_gate = PeerAtGate
         }) ->

    Transport = unknown,
    #{
       session => SessionId,
       realm => Realm,
       authid => AuthId,
       authrole => AuthRole,
       authmethod => AuthMethod,
       authprovider => AuthProvider,
       transport => Transport,
       authenticated => Authenticated,
       subscriptions => Subs,
       registrations => Regs,
       peer_at_gate => PeerAtGate
     }.

new(RealmName, Details, PeerAtGate)  ->
    Id = gen_global_id(),
    Session = #cta_session{ id = Id,
                        realm = RealmName,
                        details = Details,
                        peer_at_gate = PeerAtGate },
    try_saving_session(Session, true).


close(#cta_session{id = SessionId}) ->
    delete_by_id(SessionId).

close_all_of_realm(RealmName) ->
    {ok, Sessions} = lookup_by_realm(RealmName),
    Close = fun(Session, _) ->
                    close(Session)
                        end,
    lists:foldl(Close, ok, Sessions),
    ok.



set_auth_details(AuthId, AuthMethod, AuthProvider, Session) ->
    NewSession = Session#cta_session{ authid = AuthId,
                                  authmethod = AuthMethod,
                                  authprovider = AuthProvider},
    try_saving_session(NewSession, false).


authenticate(AuthRole, Session) ->
    NewSession = Session#cta_session{
                   authrole = AuthRole,
                   authenticated = true
                  },

    try_saving_session(NewSession, false).


add_subscription(SubId, #cta_session{subscriptions = Subs} = Session) ->
    NewSubs = [ SubId | lists:delete(SubId, Subs) ],
    NewSession = Session#cta_session{ subscriptions = NewSubs },
    try_saving_session(NewSession, false).

has_subscription(SubId, #cta_session{subscriptions = Subs}) ->
    lists:member(SubId, Subs).

get_subscriptions(#cta_session{subscriptions = Subs}) ->
    Subs.

remove_subscription(SubId, #cta_session{subscriptions = Subs} = Session) ->
    NewSubs = lists:delete(SubId, Subs),
    NewSession = Session#cta_session{ subscriptions = NewSubs },
    try_saving_session(NewSession, false).

add_registration(RegId, #cta_session{registrations = Regs} = Session) ->
    NewRegs = [ RegId | lists:delete(RegId, Regs) ],
    NewSession = Session#cta_session{ registrations = NewRegs },
    try_saving_session(NewSession, false).

has_registration(RegId, #cta_session{registrations = Regs}) ->
    lists:member(RegId, Regs).

get_registrations(#cta_session{registrations = Regs}) ->
    Regs.

remove_registration(SubId, #cta_session{subscriptions = Subs} = Session) ->
    NewSubs = lists:delete(SubId, Subs),
    NewSession = Session#cta_session{ subscriptions = NewSubs },
    try_saving_session(NewSession, false).

is_authenticated(#cta_session{authenticated = IsAuth}) ->
    IsAuth.

get_peer(#cta_session{peer_at_gate = PeerAtGate}) ->
    PeerAtGate.

get_id(#cta_session{id = Id}) ->
    Id.

get_realm(#cta_session{realm = Realm}) ->
    Realm.

get_authid(#cta_session{authid = Id}) ->
    Id.

get_authrole(#cta_session{authrole = Role}) ->
    Role.


lookup_by_realm(RealmName) ->
    Lookup = fun() ->
                     case mnesia:index_read(cta_session, RealmName, realm) of
                         Sessions when is_list(Sessions) ->
                             {ok, Sessions};
                         Error ->
                             {error, Error}
                     end
             end,
    Result = mnesia:transaction(Lookup),
    unify_result(Result).


lookup_by_id(Id) ->
    Lookup = fun() ->
                     case mnesia:read({cta_session, Id}) of
                         [Session] ->
                             {ok, Session};
                         [] ->
                             {error, not_found};
                         _ ->
                             {error, bad_state}
                     end
             end,
    Result = mnesia:transaction(Lookup),
    unify_result(Result).

try_saving_session(#cta_session{id = Id} = Session, New) ->
    Store = fun(true) ->
                    case mnesia:wread({cta_session, Id}) of
                        [] ->
                            ok = mnesia:write(Session),
                            {ok, Session};
                        _ ->
                            {error, exists}
                    end;
               (false) ->
                    ok = mnesia:write(Session),
                    {ok, Session}
            end,
    Result = mnesia:transaction(Store, [New]),
    maybe_retry_saving_session(Result, Session, New).

maybe_retry_saving_session({atomic, {ok, Session}}, _, _) ->
    {ok, Session};
maybe_retry_saving_session({atomic, {error, exists}}, Session, true) ->
    Id = gen_global_id(),
    NewSession = Session#cta_session{id = Id},
    try_saving_session(NewSession, true);
maybe_retry_saving_session(_Other, _Session, _New) ->
    {error, saving}.

delete_by_id(Id) ->
    Delete = fun() ->
                     case mnesia:wread({cta_session, Id}) of
                         [_] ->
                             mnesia:delete({cta_session, Id});
                         [] ->
                             {error, not_found}
                     end
             end,
    Result = mnesia:transaction(Delete),
    unify_result(Result).


create_table() ->
    mnesia:delete_table(cta_session),
    TabDef = [{attributes, record_info(fields, cta_session)},
              {ram_copies, [node()]},
              {index, [realm, peer_at_gate]}
             ],
    {atomic, ok} = mnesia:create_table(cta_session, TabDef),
    ok.

unify_result({atomic, Result}) ->
    Result;
unify_result(Other) ->
    Other.

gen_global_id() ->
    rand:uniform(9007199254740993) - 1.

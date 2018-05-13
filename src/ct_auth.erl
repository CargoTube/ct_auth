-module(ct_auth).

-export([handle_hello/2,
         handle_authenticate/2,
         is_message_allowed/2
        ]).

-include_lib("ct_msg/include/ct_msg.hrl").

handle_hello({hello, RealmName, Details}, Peer) ->
    Result = cta_realm:lookup(RealmName),
    SessionResult = handle_realm(Result, Details, Peer),
    send_welcome_challenge_or_abort(SessionResult, Peer).


handle_authenticate(_Authenticate, PeerAtGate) ->
    ct_router:to_peer(PeerAtGate ,{to_peer, ?ABORT(#{}, canceled)}),
    ok.

is_message_allowed(_Message, _Session) ->
    %% TODO: implement
    true.

handle_realm({ok, Realm}, Details, Peer) ->
    AuthMethod = get_auth_method(Realm, Details),
    maybe_create_session(AuthMethod, Realm, Details, Peer);
handle_realm(_Result, _Details, _Peer) ->
    {error, no_such_realm}.

maybe_create_session(none, _Realm, _Details, _Peer) ->
    {error, no_such_auth_method};
maybe_create_session(AuthMethod, Realm, Details, Peer) ->
    RealmName = cta_realm:get_name(Realm),
    {ok, Session} = cta_session:new(RealmName, Details, Peer),
    {ok, Session, AuthMethod, Realm}.


get_auth_method(Realm, Details) ->
    AuthId = maps:get(<<"authid">>, Details, undefined),
    AuthMethods = get_client_authmethods(Details, AuthId),
    RealmMethods = cta_realm:get_auth_methods(Realm),

    ToAtom =
        fun(Method, List) ->
                try
                    [ binary_to_existing_atom(Method, utf8) | List ]
                catch _:_ ->
                        List
                end
        end,
    ClientSupported = lists:foldl(ToAtom, [], AuthMethods),

    FindBestMethod =
        fun(Method, Current) ->
                case lists:member(Method, ClientSupported) of
                    true ->
                        Method;
                    false ->
                        Current
                end
        end,
    lists:foldr(FindBestMethod, none, RealmMethods).


get_client_authmethods(Details, undefined) ->
    maps:get(<<"authmethods">>, Details, [<<"anonymous">>]);
get_client_authmethods(Details, _) ->
    maps:get(<<"authmethods">>, Details, []).


send_welcome_challenge_or_abort({ok, Session, anonymous, Realm}, _Peer) ->
    {ok, NewSession} = cta_session:set_auth_details(anonymous, anonymous,
                                                    anonymous, Session),
    RoleResult = cta_realm:get_role(anonymous, Realm),
    maybe_authenticate_session(RoleResult, NewSession);
send_welcome_challenge_or_abort({error, no_such_realm}, PeerAtGate) ->
    send_abort(PeerAtGate, no_such_realm);
send_welcome_challenge_or_abort({error, no_such_auth_method}, PeerAtGate) ->
    send_abort(PeerAtGate, invalid_argument);
send_welcome_challenge_or_abort({ok, Session, _, _}, _PeerAtGate) ->
    abort_session(Session);
send_welcome_challenge_or_abort( _, PeerAtGate) ->
    send_abort(PeerAtGate, canceled).

maybe_authenticate_session({ok, Role}, Session) ->
    {ok, NewSession} = cta_session:authenticate(Role, Session),
    SessionId = cta_session:get_id(NewSession),
    Details = #{
      agent => ct_router:agent_identification(),
      roles => ct_router:agent_roles()
     },
    ct_router:to_session(Session,?WELCOME( SessionId, Details)),
    ok;
maybe_authenticate_session(_, Session) ->
    abort_session(Session).


abort_session(Session) ->
    Peer = cta_session:get_peer(Session),
    ok = cta_session:close(Session),
    send_abort(Peer, canceled).

send_abort(Peer, Reason) ->
    ct_router:to_peer(Peer, {to_peer, ?ABORT(#{}, Reason)}),
    ok.
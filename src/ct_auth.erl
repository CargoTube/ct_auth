-module(ct_auth).

-export([handle_hello/3,
         handle_authenticate/2,
         is_message_allowed/2,

         init/0
        ]).

-include("ct_auth.hrl").

init() ->
    ct_data_util:create_schema_if_needed(),
    cta_session:init(),
    cta_session:init(),
    ok.


handle_hello({hello, RealmName, Details}, Peer, Transport) ->
    Result = cta_realm:lookup(RealmName),
    SessionResult = handle_realm(Result, Details, Peer, Transport),
    return_welcome_challenge_or_abort(SessionResult).


handle_authenticate(_Authenticate, _PeerAtGate) ->
    {abort, canceled}.

is_message_allowed(_Message, _Session) ->
    %% TODO: implement
    true.

handle_realm({ok, Realm}, Details, Peer, Transp) ->
    IsClosing = cta_realm:is_closing(Realm),
    AuthMethod = get_auth_method(Realm, Details),
    maybe_create_session(IsClosing, AuthMethod, Realm, Details, Peer, Transp);
handle_realm(_Result, _Details, _Peer, _Transport) ->
    {error, no_such_realm}.

maybe_create_session(true, _Methods, _Realm, _Details, _Peer, _Transport) ->
    {error, realm_closing};
maybe_create_session(false, none, _Realm, _Details, _Peer, _Transport) ->
    {error, no_such_auth_method};
maybe_create_session(false, AuthMethod, Realm, Details, Peer, Transport) ->
    RealmName = cta_realm:get_name(Realm),
    {ok, Session} = cta_session:new(RealmName, Details, Peer, Transport),
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


return_welcome_challenge_or_abort({ok, Session, anonymous, Realm}) ->
    {ok, NewSession} = cta_session:set_auth_details(anonymous, anonymous,
                                                    anonymous, Session),
    RoleResult = cta_realm:get_role(anonymous, Realm),
    maybe_authenticate_session(RoleResult, NewSession);
return_welcome_challenge_or_abort({error, realm_closing}) ->
    return_abort(close_realm);
return_welcome_challenge_or_abort({error, no_such_realm}) ->
    return_abort(no_such_realm);
return_welcome_challenge_or_abort({error, no_such_auth_method}) ->
    return_abort(invalid_argument);
return_welcome_challenge_or_abort({ok, Session, _, _}) ->
    abort_session(Session);
return_welcome_challenge_or_abort( _) ->
    return_abort(canceled).

maybe_authenticate_session({ok, Role}, Session) ->
    cta_session:authenticate(Role, Session);
maybe_authenticate_session(_, Session) ->
    abort_session(Session).


abort_session(Session) ->
    ok = cta_session:close(Session),
    return_abort(canceled).

return_abort(Reason) ->
    {abort, Reason}.

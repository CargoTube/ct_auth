
-record(cta_realm, {name = undefined,
                    authmethods = [],
                    authmapping = [],
                    is_closing = false
                   }).

-record(cta_session, {
          id = undefined,
          realm = undefined,
          details = #{},
          authid = undefined,
          authrole = undefined,
          authprovider = undefined,
          authmethod = undefined,
          authenticated = false,
          subscriptions = [],
          registrations = [],
          peer_at_gate = undefined
         }).

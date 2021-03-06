
-record(cta_realm, {name = undefined,
                    authmethods = [],
                    authmapping = [],
                    is_closing = false
                   }).

-record(cta_session, {
          id = undefined,
          realm = undefined,
          details = #{},
          disclose_caller = false,
          disclose_publisher = false,
          authid = undefined,
          authrole = undefined,
          authprovider = undefined,
          authmethod = undefined,
          authenticated = false,
          subscriptions = [],
          registrations = [],
          transport = #{},
          peer_at_gate = undefined
         }).

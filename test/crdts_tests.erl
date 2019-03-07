-module(crdts_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Unit Tests
%%====================================================================
dummy_test_() ->
    [
     {"dummy test",
      fun () ->
              A = 1,
              B = 1,
              ?assertEqual(A, B)
      end}
    ].

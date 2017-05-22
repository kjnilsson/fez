-module(aether_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

lens_test() ->
    "Goodbye World!" = aether_test:lens().

-module(prop_token).
-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?SETUP(fun() ->
                   {ok, Apps} = application:ensure_all_started(idp),
                   fun() -> [application:stop(App) || App <- Apps], ok end
           end,
           ?FORALL(Int, integer(),
                   begin
                      Code = idp_rp:create_code(Int),
                      io:format("Int: ~p, Code: ~p",[Int,Code]),
                      code_length(Code,Int)
                   end)
          ).
%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
code_length(Code,Len) -> length(Code) == Len.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

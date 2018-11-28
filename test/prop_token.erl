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
           ?FORALL(Int, atom(),
                   begin
                       {ok,[]} = idp_mng:get_rps(),
                       idp_mng:register_rp(Int),
                       {ok,{A,_}} = idp_mng:get_rp(Int),
                       idp_mng:unregister_rp(Int),
                        A == Int
                   end)
          ).


prop_consent() ->
    %% pick a list and a last number
    ?SETUP(fun() ->
                   {ok, Apps} = application:ensure_all_started(idp),
                   fun() -> [application:stop(App) || App <- Apps], ok end
           end,
           ?FORALL(Consent, atom(),
                   begin
                       idp_mng:register_rp(test),
                       idp_mng:add_consent(test,Consent), 
                       {ok,Consents} = idp_mng:get_consents(test),
                       idp_mng:remove_consent(test,Consent),
                       idp_mng:unregister_rp(test),
                       io:format("~p in ~p~n",[Consent,Consents]),
                       lists:member(Consent,Consents)
                   end)
          ).

%prop_user() ->
    %%% pick a list and a last number
    %?SETUP(fun() ->
                   %{ok, Apps} = application:ensure_all_started(idp),
                   %fun() -> [application:stop(App) || App <- Apps], ok end
           %end,
           %?FORALL({User,Pass}, {non_empty(utf8()),non_empty(utf8())},
                   %begin
                       %idp_mng:reg_user(binary_to_list(User),binary_to_list(Pass)),
                       %BinUser = erlang:binary_to_atom(User,utf8),
                       %Result = idp_mmg:validate_user(BinUser,Pass),
                       %Result == {ok, true}
                   %end)
          %).
%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
code_length(Code,Len) -> length(Code) == Len.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

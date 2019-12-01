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
                       lists:member(Consent,Consents)
                   end)
          ).

prop_rp() ->
    %%% pick a list and a last number
    ?SETUP(fun() ->
                   {ok, Apps} = application:ensure_all_started(idp),
                   fun() -> [application:stop(App) || App <- Apps], ok end
           end,
           ?FORALL(RPName, non_empty(list(lett())),
                   begin
                       Result = idp_mng:register_rp(RPName),
                       idp_mng:unregister_rp(RPName),
                       %BinUser = erlang:binary_to_atom(User,utf8),
                       %Result = idp_mmg:validate_user(BinUser,Pass),
                       {Rs1,{Rs2,_}} = Result,
                       (Rs1 == ok) and (Rs2 == RPName)
                   end)
          ).

prop_user_login() ->
    ReDir = "http://redir/",
    RPName = test,
    ?SETUP(fun() ->
                   {ok, Apps} = application:ensure_all_started(idp),
                   fun() -> [application:stop(App) || App <- Apps], ok end
           end,
           ?FORALL({User,Pass,CSize}, {non_empty(list(lett())),non_empty(list(lett())),pos_integer()},
                   begin
                       io:format("~p",[CSize]),
                       AtUser = list_to_atom(User),
                       idp_usermng:reg_user(User,Pass),
                       {ok, {RP_Client,RP_Secret}} = idp_mng:register_rp(RPName),
                       Validate = idp_mng:validate_rp(RP_Client,RP_Secret),
                       idp_mng:set_rp_code_size(RP_Client,CSize), 

                       Result = idp_mng:validate_user(AtUser,Pass),

                       {Res, Code} = idp_mng:authorize(RPName,ReDir,AtUser),
                       {Res2, Token} = idp_mng:validate_code(RPName,Code,ReDir),

                       idp_mng:unregister_rp(RPName),
                       idp_usermng:unreg_user(User,Pass),

                       (Result == {ok,true}) and (Res == ok) and (Res2 == ok) and (Validate == {ok, true}) and (length(Code) == calc_base64_len(CSize))
                   end)
          ).


calc_base64_len(N) when N rem 3 == 0 ->
    4*(N div 3);
calc_base64_len(N)  ->
    round((4*(N/3)))+ 4-(round(4*(N/3)) rem 4).
%2> idp_mng:reg_user("viktor","test").
%ok
%%3> idp_mng:authorize(test,"http://redir/",viktor).
%{ok,"-Tb4fO5uihnGFJrJF-IEkqLw0gTeqPx3hl4ta-VTzUQ="}
%4> idp_mng:validate_  
%validate_code/3  validate_rp/2    validate_user/2  
%4> idp_mng:validate_code(test,"-Tb4fO5uihnGFJrJF-IEkqLw0gTeqPx3hl4ta-VTzUQ=","http://redir/").
%{ok,#{<<"access_token">> =>
          %%<<"gsVRyvhs_MwGLFUF3ltFLskV8medSsD64xuJdEjjC5gKkHM_imudP4HxNwP9mh5jDkdCW6LKXdx0EkDZLStcor3U7WnCRBH_IBmKbxXZ"...>>,
      %%<<"expires_in">> => <<"3600">>,
      %<<"refresh_token">> =>
          %<<"lf5xikhOyMRoMNSQQxD401pgUYGkTEmB7T1PiOnaAjVyxDlGr61_M4PZoueInv67EwYtenAgJb3CJA95mZHKMgWR7bTbXQ6g7JrRi5D5"...>>,
      %<<"token_type">> => <<"Bearer">>}}
%5> 
%
%
%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
code_length(Code,Len) -> length(Code) == Len.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
ascii_string() ->non_empty(list(lett())).
lett() -> oneof([range($a,$z),range($A,$Z)]).

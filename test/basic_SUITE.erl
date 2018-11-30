-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,end_per_group/2,auth_user/1,init_per_group/2,init_per_testcase/2,groups/0]).
-export([create_user/1,get_rps/1, create_rp/1,validate_user/1]).
 
all() -> [{group,rp_validate},{group,create_validate_user}].

groups() -> [{create_validate_user,
              [sequence],
              [create_user,validate_user]},
             {rp_validate,
              [],
              [get_rps,create_rp,auth_user]}].


init_per_testcase(_A, Config) ->
    Config.

init_per_group(_, Config) ->
    {ok, _} = application:ensure_all_started(idp),
    Username = "test_user",
    Password = "test_password",
    idp_mng:reg_user(Username, Password),
    {ok,{test_client,_Code}} = idp_mng:register_rp(test_client),
    {ok,A} = idp_mng:get_rps(),
    io:format("rps:~p~n",[A]),
    Config.


end_per_group(_, _Config) ->
    ok = idp_mng:unregister_rp(test_client),
    Username = "test_user",
    Password = "test_password",
    idp_mng:unreg_user(Username, Password),
    ok.

%%%%%%%%%%%
%%%GÖR EN UNREGISTER_USER FUNKTION!!


%%Group user
create_user(_Config) -> 
    Username = "test_user2",
    Password = "test_password",
    ok = idp_mng:reg_user(Username, Password).

validate_user(_Config) ->
    Username = "test_user2",
    Password = "test_password",
    BinUser = list_to_binary(Username),
    idp_mng:validate_user(BinUser,Password).
    

%% Group RP
get_rps(_Config) ->
    {ok,[]} == idp_mng:get_rps().
 
create_rp(_Config) ->
    {ok,{test_client2,_Code}} = idp_mng:register_rp(test_client2),
    %{ok,[RP]} = idp_mng:get_rps(),
    ok = idp_mng:unregister_rp(test_client2).

auth_user(_Config) -> 
    %{ok,{test_client,_Code}} = idp_mng:register_rp(test_client),
    ClientId = test_client,
    UserId = test_user,
    Redir = <<"https://test.com/">>,
    {ok,Code} = idp_mng:authorize(ClientId,Redir,UserId),
    idp_mng:validate_code(ClientId,Code,Redir).




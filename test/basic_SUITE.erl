-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,end_per_group/2,init_per_group/2,init_per_testcase/2,groups/0]).
-export([create_user/1,get_rps/1, create_rp/1,validate_user/1]).
 
all() -> [{group,rp_validate},{group,create_validate_user}].

groups() -> [{create_validate_user,
              [sequence],
              [create_user,validate_user]},
             {rp_validate,
              [],
              [get_rps,create_rp]}].

init_per_testcase(_A, Config) ->
    {ok, _} = application:ensure_all_started(idp),
    Config.

init_per_group(_, Config) ->
    {ok, _} = application:ensure_all_started(idp),
    Config.
end_per_group(_, _Config) ->
    ok.
create_user(_Config) -> 
    Username = "viktor",
    Password = "test",
    ok = idp_mng:reg_user(Username, Password).

validate_user(_Config) ->
    Username = "viktor",
    Password = "test",
    BinUser = list_to_binary(Username),
    idp_mng:validate_user(BinUser,Password).
    
get_rps(_Config) ->
    {ok,[]} == idp_mng:get_rps().
 
create_rp(_Config) ->
    {ok,{test,_Code}} = idp_mng:register_rp(test),
    {ok,[RP]} = idp_mng:get_rps(),
    ok = idp_mng:unregister_rp(test),
    RP == test.

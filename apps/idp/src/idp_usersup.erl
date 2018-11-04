%%%-------------------------------------------------------------------
%% @doc idp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(idp_usersup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_child() ->
  supervisor:start_child(?SERVER, []).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {'idp_user', {'idp_user', start_link, []}, Restart, Shutdown, Type, ['idp_user']},

    %User = {'idp_user', {'idp_user', start_link, []}, Restart, Shutdown, TypeW, ['idp_user']},
    {ok, { {SupFlags}, [AChild]} }.

%%====================================================================
%% Internal functions
%%====================================================================
%%
%%
%%

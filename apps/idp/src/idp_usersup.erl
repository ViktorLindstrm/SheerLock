%%%-------------------------------------------------------------------
%% @doc idp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(idp_usersup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Restart = permanent,
    Shutdown = 2000,
    TypeW = worker,

    User = {'idp_user', {'idp_user', start_link, []}, Restart, Shutdown, TypeW, ['idp_user']},
    {ok, { {one_for_all, 0, 1}, [User]} }.

%%====================================================================
%% Internal functions
%%====================================================================

%%%-------------------------------------------------------------------
%% @doc idp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(idp_sup).

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
    TypeS = supervisor,

    UserMng = {'idp_usermng', {'idp_usermng', start_link, []}, Restart, Shutdown, TypeW, ['idp_usermng']},
    IDPMng = {'idp_mng', {'idp_mng', start_link, []}, Restart, Shutdown, TypeW, ['idp_mng']},
    %Users = {'idp_usersup', {'idp_usersup', start_link, []}, Restart, Shutdown, TypeS, ['idp_usersup']},
    RP    = {'idp_rpsup',   {'idp_rpsup', start_link, []}, Restart, Shutdown, TypeS, ['idp_rpsup']},
    UserSup    = {'idp_usersup',   {'idp_usersup', start_link, []}, Restart, Shutdown, TypeS, ['idp_usersup']},
    {ok, { {one_for_all, 0, 1}, [IDPMng,RP,UserMng,UserSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================

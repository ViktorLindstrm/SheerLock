-module(idp_user).
-behaviour(gen_server).
-export([
         start_link/1,
         reg_user/2,
         verify/2,
         set_code/2,
         set_token/2,
         expire_token/1,
         get_userviatoken/1,
         get_scopes/1,
         add_scope/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {id,
               session = undefined,
               username         :: atom(),
               name             :: list(),
               password         :: list(),
               scopes = []      :: list(),
               token = undefined,
               code = undefined :: 'undefined' | list()
              }).

-record(user, {id,
               username         :: atom(),
               name             :: list(),
               password         :: list(),
               scopes = []      :: list(),
               token = undefined,
               code = undefined :: 'undefined' | list()
              }).

-record(token, {access_token    :: 'undefined' | list(),
                token_type      :: list(),
                expires_in      :: list(),
                expired = true  :: boolean(),
                refresh_token   :: 'undefined' | binary(),
                scopes,
                created_time    :: list()
               }).

-include_lib("stdlib/include/ms_transform.hrl").
-define(SERVER, ?MODULE).

start_link(UserData) ->
    gen_server:start_link(?MODULE, [UserData], []).

init([UserData]) ->
    Id = UserData#user.id,
    Username = UserData#user.username,
    Name = UserData#user.name,
    Password = UserData#user.password,
    Scopes = UserData#user.scopes,
    Token = UserData#user.token,

    {ok, #state{id=Id,username = Username,name = Name, password = Password, scopes = Scopes, token = Token}}.

handle_call({verify,InpPassword}, _From, #state{password = Password} = State) ->
    Reply = {ok, Password == hash_pw(InpPassword)},
    {reply, Reply, State};

handle_call({set_code,Code}, _From, State) ->
    Reply = ok,
    NewState = State#state{code=Code},
    {reply, Reply, NewState};

handle_call({get_scopes}, _From,  #state{scopes=Scopes} = State) ->
    Reply = {ok, Scopes}, 
    {reply, Reply, State};

handle_call({add_scope,Scope}, _From, #state{scopes=Scopes} = State) ->
    NewState = State#state{scopes=[Scope|Scopes]},
    Reply = ok,
    {reply, Reply, NewState};


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
reg_user(Username,Password) -> gen_server:call(?MODULE,{register,{Username,Password}}).
verify(Username,Password) -> gen_server:call(?MODULE,{verify,{Username,Password}}).
set_code(User,Code) -> gen_server:call(?MODULE,{set_code,{User,Code}}).
set_token(Code,Token) -> gen_server:call(?MODULE,{set_token,{Code,Token}}).
expire_token(Token) -> gen_server:call(?MODULE,{expire_token,Token}).
get_userviatoken(Token) -> gen_server:call(?MODULE,{get_userviatoken,Token}).
get_scopes(UserId) -> gen_server:call(?MODULE,{get_scopes,UserId}).
add_scope(UserId,Scope) -> gen_server:call(?MODULE,{add_scope,{UserId,Scope}}).

hash_pw(Password) -> 
    crypto:hash(sha256,Password).

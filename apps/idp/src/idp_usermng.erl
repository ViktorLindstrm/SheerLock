-module(idp_usermng).
-behaviour(gen_server).
-export([
         start_link/0,
         reg_user/2,
         verify/2,
         set_code/2,
         set_session/2,
         expire_session/3,
         set_token/2,
         expire_token/1,
         get_userviatoken/1,
         get_userviasession/1,
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

-record(state, {id = undefined,users=undefined}).
-record(user, {id,
               session          :: list(),
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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{users=ets:new(users,[set])}}.

handle_call({verify,{Username,Password}}, _From, #state{users = Users} = State) ->
    Reply = case get_user(Username,Users) of 
                {ok,PID} -> 
                    gen_server:call(PID,{verify,Password});
                {error,_} -> 
                    {error, no_such_user}
            end,
    {reply, Reply, State};

handle_call({set_code,{UserID,Code}}, _From, #state{users = Users} = State) ->
    Reply = case get_user(UserID,Users) of 
                {ok,PID} -> 
                    gen_server:call(PID,{set_code,Code});
                {error,_} -> 
                    {error, no_such_user}
            end,
    {reply, Reply, State};

handle_call({get_userviasession,SID}, _From, #state{users = Users} = State) ->
    logger:debug("get_userviasession, SID: ~p",[SID]),
    Reply = case ets:select(Users, ets:fun2ms(fun(N = {_,#user{session=Session}}) when Session == SID -> N end)) of 
              [{_,#user{id = MaybePID}}] when is_pid(MaybePID)->
                  {ok,MaybePID};
              [{_,U}] ->
                  {ok,PID} = idp_usersup:start_child(U),
                  ets:insert(Users,{U#user.username,U#user{id=PID}}),
                  {ok,PID};
              [] ->
                  {error, no_such_user}
          end,
    {reply, Reply, State};

handle_call({get_userviatoken,Token}, _From, #state{users = Users} = State) ->
    Reply = get_userviatoken(Token,Users),
    {reply, Reply, State};

handle_call({set_session,{UserId,SessionTime}}, _From, #state{users = Users} = State) ->
    {ok,UserPID} = get_user(UserId,Users),
    {ok,SessionId} = gen_server:call(UserPID,{set_session}),
    logger:debug("SessionID : ~p",[SessionId]),
    logger:debug("Setting session for user ~p~nSessionID ~p",[UserId,SessionId]),
    Ret = case ets:select(Users, ets:fun2ms(fun(N = {_,#user{username=ID}}) when ID == UserId -> N end)) of
        [{_,U}] ->
            ets:insert(Users,{U#user.username,U#user{session=SessionId}}),
            ok;
        [] ->
            {error, no_such_user}
    end,
    logger:debug("Set session ret: ~p",[Ret]),
    Reply = {ok,SessionId},
    %erlang:spawn(?MODULE,expire_session,[UserPID,SessionTime,Users]),
    %Reply = gen_server:call(UserPID,{set_session}),
    {reply, Reply, State};

handle_call({expire_token,Token}, _From, #state{users = Users} = State) ->
    {ok,UserPid} = get_userviatoken(Token,Users),
    gen_server:call(UserPid,{expire_token,Token}),
    Reply = expire_token(Token,Users),
    {reply, Reply, State};

handle_call({set_token,{UserID,Token}}, _From, #state{users = Users} = State) ->
    logger:debug("set_token: ~nUser: ~p~nToken: ~p",[UserID,Token]),
    {ok,PID} = get_user(UserID,Users),
    set_token(UserID,Token,Users),
    %ets:insert(Users,{U#user.username,U#user{token=Token}}),
    Reply = gen_server:call(PID,{set_token,Token}),
    {reply, Reply, State};

handle_call({register,{Username,Password}}, _From, #state{users = Users} = State) ->
    Reply = case get_user(list_to_atom(Username),Users) of 
                           {error,no_such_user} -> 
                               NewUser = #user{id = crypto:strong_rand_bytes(128),
                                               username=list_to_atom(Username),
                                               password=hash_pw(Password)},
                               set_user(NewUser,Users),
                               ok;
                           {ok,_} -> 
                               {error, user_exist}
                       end,
    {reply, Reply, State};

handle_call({get_scopes,UserID}, _From, #state{users = Users} = State) ->
    Reply = case get_user(UserID,Users) of 
                {ok,PID} -> 
                    {ok, _Scopes} = gen_server:call(PID,{get_scopes});
                {error,_} -> 
                    {error, no_such_user};
                E -> 
                    io:format("Error!: ~p ~n",[E]),
                    {error, E}
            end,
    {reply, Reply, State};

handle_call({add_scope,{UserID,Scope}}, _From, #state{users = Users} = State) ->
    Reply = case get_user(UserID,Users) of 
                {ok,PID} -> 
                    gen_server:call(PID,{add_scope,Scope});
                    %add_scope(Userdata,Scope,Users);
                {error,_} -> 
                    {error, no_such_user}
            end,
    {reply, Reply, State};


handle_call(Request, _From, State) ->
    logger:debug("Ignored : ~p",[Request]),
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
get_userviasession(Session) -> gen_server:call(?MODULE,{get_userviasession,Session}).
get_scopes(UserId) -> gen_server:call(?MODULE,{get_scopes,UserId}).
add_scope(UserId,Scope) -> gen_server:call(?MODULE,{add_scope,{UserId,Scope}}).
set_session(UserId,SessionTime) -> gen_server:call(?MODULE,{set_session,{UserId,SessionTime}}).

get_user(UserID,Users) ->
    Reply = case ets:select(Users, ets:fun2ms(fun(N = {_,#user{username=ID}}) when ID == UserID -> N end)) of
                 [{_,#user{id = MaybePID}}] when is_pid(MaybePID)->
                     {ok,MaybePID};
                 [{_,U}] ->
                    {ok,PID} = idp_usersup:start_child(U),
                    ets:insert(Users,{U#user.username,U#user{id=PID}}),
                    {ok,PID};
                 [] ->
                     {error, no_such_user}
             end,
     Reply.
 
set_user(User,Users) ->
    ets:insert(Users,{User#user.username,User}).

add_scope(User,Scope,Users) ->
    ets:insert(Users,{User#user.username,User#user{scopes=[Scope|User#user.scopes]}}).

set_token(UserID,Token,Users) ->
    Reply = case ets:select(Users, ets:fun2ms(fun(N = {_,#user{username=ID}}) when ID == UserID -> N end)) of
                 [{_,U}] ->
                    ets:insert(Users,{U#user.username,U#user{token={Token#token.access_token,Token}}}),
                    ok;
                 [] ->
                     {error, no_such_user}
             end,
     Reply.

expire_token(Token,Users) ->
    case ets:select(Users, ets:fun2ms(fun(N = {_,#user{token={AT,_}}}) when AT == Token -> N end)) of 
        [{_,User}] ->
            io:format("User: ~p~n",[User]),
            {_Access,T} = User#user.token,
            NewToken = T#token{access_token=undefined,expired=true},
            ets:insert(Users,{User#user.username,User#user{token={NewToken#token.access_token,NewToken}}}),
            {ok,User#user.username};
        [] ->
            {error,no_such_user}
    end.
    
get_userviatoken(Token,Users) ->
    logger:debug("get_userviatoken: ~p",[Token]),
    case ets:select(Users, ets:fun2ms(fun(N = {_,#user{token={AT,_}}}) when AT == Token -> N end)) of 
        [{_,#user{id = MaybePID}}] when is_pid(MaybePID)->
            {ok,MaybePID};
        [{_,U}] ->
            {ok,PID} = idp_usersup:start_child(U),
            ets:insert(Users,{U#user.username,U#user{id=PID}}),
            {ok,PID};
        [] ->
            {error, no_such_user}
    end.


expire_session(UserPID,SessionTime,Users) ->
    timer:sleep(SessionTime),
    Ret = case ets:select(Users, ets:fun2ms(fun(N = {_,#user{id=Pid}}) when Pid == UserPID -> N end)) of 
        [{_,User}] ->
            logger:debug("Expire session User: ~p~nin talbe: ~p",[User,Users]),
            ets:insert(Users,{User#user.username,User#user{session=undefined}}),
            {ok,User#user.username};
        [] ->
            {error,no_such_user}
    end,
    gen_server:call(UserPID,{expire_session}),
    Ret.


hash_pw(Password) -> 
    crypto:hash(sha256,Password).



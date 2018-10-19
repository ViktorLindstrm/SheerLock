-module(idp_user).
-behaviour(gen_server).
-export([
         start_link/0,
         reg_user/2,
         verify/2,
         set_code/2,
         set_token/2,
         expire_token/1,
         get_userviatoken/1,
         get_consent/1
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
               username         :: atom(),
               name             :: list(),
               password         :: list(),
               consents         :: list(),
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
                {ok,Userdata} -> 
                    {ok, Userdata#user.password == hash_pw(Password)};
                {error,_} -> 
                    {error, no_such_user}
            end,
    {reply, Reply, State};

handle_call({set_code,{UserID,Code}}, _From, #state{users = Users} = State) ->
    Reply = case get_user(UserID,Users) of 
                {ok,Userdata} -> 
                    set_code(Userdata,Code,Users);
                {error,_} -> 
                    {error, no_such_user}
            end,
    {reply, Reply, State};

handle_call({get_userviatoken,Token}, _From, #state{users = Users} = State) ->
    Reply = get_userviatoken(Token,Users),
    {reply, Reply, State};
handle_call({expire_token,Token}, _From, #state{users = Users} = State) ->
    Reply = expire_token(Token,Users),
    {reply, Reply, State};

handle_call({set_token,{Code,Token}}, _From, #state{users = Users} = State) ->
    Reply = set_token(Code,Token,Users),
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

handle_call({get_consent,UserID}, _From, #state{users = Users} = State) ->
    Reply = case get_user(UserID,Users) of 
                {ok,#user{consents = Consents}} -> 
                    {ok, Consents};
                {error,_} -> 
                    {error, no_such_user}
            end,
    {reply, Reply, State};

handle_call({add_consent,{UserID,Consent}}, _From, #state{users = Users} = State) ->
    Reply = case get_user(UserID,Users) of 
                {ok,Userdata} -> 
                    add_consent(Userdata,Consent,Users);
                {error,_} -> 
                    {error, no_such_user}
            end,
    {reply, Reply, State};


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
get_consent(UserId) -> gen_server:call(?MODULE,{get_consent,UserId}).

get_user(UserID,Users) ->
    Reply = case ets:select(Users, ets:fun2ms(fun(N = {_,#user{username=ID}}) when ID == UserID -> N end)) of
                 [{_,U}] ->
                     {ok,U};
                 [] ->
                     {error, no_such_user}
             end,
     Reply.
 
set_user(User,Users) ->
    ets:insert(Users,{User#user.username,User}).

add_consent(User,Consent,Users) ->
    ets:insert(Users,{User#user.username,User#user{consents=User#user.consents++Consent}}).

set_code(User,Code,Users) ->
    ets:insert(Users,{User#user.username,User#user{code=Code}}).

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
    case ets:select(Users, ets:fun2ms(fun(N = {_,#user{token={AT,_}}}) when AT == Token -> N end)) of 
        [{_,User}] ->
            {ok,User};
        [] ->
            {error,no_such_user}
    end.

set_token(Code,Token,Users) ->
    case ets:select(Users, ets:fun2ms(fun(N = {_,#user{code=UCode}}) when UCode == Code -> N end)) of 
        [{_,User}] ->
            ets:insert(Users,{User#user.username,User#user{code=undefined,token={Token#token.access_token,Token}}}),
            ok;
        [] ->
            {error,no_such_user}
    end.

hash_pw(Password) -> 
    crypto:hash(sha256,Password).
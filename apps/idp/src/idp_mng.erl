-module(idp_mng).
-behaviour(gen_server).
-export([
         start_link/0,
         register_rp/1,
         get_rps/0,
         get_scopes/1,
         get_consent/2,
         get_unconfirmed_scopes/1,
         add_scope/2,
         remove_scope/2,

         get_consents/1,
         add_consent/2,
         remove_consent/2,
         add_scope_to_consent/3,
         remove_scope_to_consent/3,

         get_rp/1,
         set_rp_pass/2,
         authorize/3,
         validate_code/3,
         unregister_rp/1,
         reg_user/2,
         validate_user/2,
         get_userinfo/1,
         validate_rp/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {rps=[]}).
-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({register_rp,RPId}, _From, #state{rps = RPS} = State) ->
    {Reply,NewState} = case lists:keyfind(RPId,1,RPS) of
                           false ->
                               {ok,RPPid} = idp_rpsup:start_child(RPId),
                               NewRPS = lists:keystore(RPId, 1, RPS, {RPId,RPPid}),
                               RP_Reply = gen_server:call(RPPid,{get_pwid}),
                               {RP_Reply,State#state{rps=NewRPS}};
                           _ ->
                               {{error, already_registered},State}
                       end,
    {reply, Reply, NewState};

handle_call({get_rp,RPId}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                           {RPId,RPPid} ->
                               _RP_Reply = gen_server:call(RPPid,{get_pwid});
                           _ ->
                               {error, not_registered}
                       end,
    {reply, Reply, State};

handle_call({get_rps}, _From, #state{rps = RPS} = State) ->
    Reply = {ok,[A||{A,_} <- RPS]},
    {reply, Reply, State};

handle_call({validate_rp,{RPId,Pass}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                           {RPId,RPPid}->
                               RP_Reply = gen_server:call(RPPid,{validate_rp,Pass}),
                               RP_Reply;
                           _ ->
                               {error, not_registered}
                       end,
    {reply, Reply, State};

handle_call({set_rp_pass,{RPId,Pass}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                           {RPId,RPPid}->
                               RP_Reply = gen_server:call(RPPid,{set_rp_pass,Pass}),
                               RP_Reply;
                           _ ->
                               {error, not_registered}
                       end,
    {reply, Reply, State};

handle_call({unregister_rp,RPId}, _From, #state{rps = RPS} = State) ->
    {Reply,NewState} = case lists:keyfind(RPId,1,RPS) of
                           {RPId,RPPid} ->
                               ok = gen_server:stop(RPPid),
                               NewRPS = lists:keydelete(RPId, 1, RPS),
                               {ok,State#state{rps=NewRPS}};
                           false ->
                               {{error, not_registered},State}
                       end,
    {reply, Reply, NewState};

handle_call({validate_code,{RPId,Code,RedirectUri}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    gen_server:call(RPPid,{validate_code,{Code,RedirectUri}});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};

handle_call({authorize,{RPId,RedirectUri,UserId}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    {ok,_Code} = gen_server:call(RPPid,{authorize,RedirectUri,UserId});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};

handle_call({get_consent,{RPId,Consent}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    {ok,_Scopes} = gen_server:call(RPPid,{get_consent,Consent});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};

handle_call({get_consents,RPId}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    {ok,_Scopes} = gen_server:call(RPPid,{get_consents});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};


handle_call({add_scope,{RPId,Scope}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    gen_server:call(RPPid,{add_scope,Scope});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};

handle_call({remove_scope,{RPId,Scope}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    gen_server:call(RPPid,{remove_scope,Scope});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};


handle_call({add_consent,{RPId,Consent}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    gen_server:call(RPPid,{add_consent,Consent});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};

handle_call({remove_consent,{RPId,Consent}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    gen_server:call(RPPid,{remove_consent,Consent});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};

handle_call({add_scope_to_consent,{RPId,Scope,Consent}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    gen_server:call(RPPid,{add_scope_to_consent,{Consent,Scope}});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};

handle_call({remove_scope_to_consent,{RPId,Scope,Consent}}, _From, #state{rps = RPS} = State) ->
    Reply = case lists:keyfind(RPId,1,RPS) of
                {RPId,RPPid} ->
                    gen_server:call(RPPid,{remove_scope_to_consent,{Consent,Scope}});
                false ->
                    {error, no_such_rp}
            end,
    {reply, Reply, State};


handle_call(Request, _From, State) ->
    io:format("Error!! ~n~p~n",[Request]),
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
register_rp(RPId) -> gen_server:call(?MODULE,{register_rp,RPId}).
get_rps() -> gen_server:call(?MODULE,{get_rps}).
get_rp(RPId) -> gen_server:call(?MODULE,{get_rp,RPId}).
validate_rp(RPId,Pass) -> gen_server:call(?MODULE,{validate_rp,{RPId,Pass}}).
unregister_rp(RPId) -> gen_server:call(?MODULE,{unregister_rp,RPId}).

get_scopes(RPId) -> gen_server:call(?MODULE,{get_scopes,RPId}).
get_consent(RPId,Consent) -> gen_server:call(?MODULE,{get_consent,{RPId,Consent}}).
get_unconfirmed_scopes(RPId) -> gen_server:call(?MODULE,{get_unconfirmed_scopes,RPId}).
add_scope(RPId,Scope) -> gen_server:call(?MODULE,{add_scope,{RPId,Scope}}).
remove_scope(RPId,Scope) -> gen_server:call(?MODULE,{remove_scope,{RPId,Scope}}).

get_consents(RPId) -> gen_server:call(?MODULE,{get_consents,RPId}).
add_consent(RPId,Consent) -> gen_server:call(?MODULE,{add_consent,{RPId,Consent}}).
remove_consent(RPId,Consent) -> gen_server:call(?MODULE,{remove_consent,{RPId,Consent}}).

add_scope_to_consent(RPId,Scope,Consent) ->
    remove_scope_to_consent(RPId,Scope,unconfirmed),
    gen_server:call(?MODULE,{add_scope_to_consent,{RPId,Scope,Consent}}).
remove_scope_to_consent(RPId,Scope,Consent) ->gen_server:call(?MODULE,{remove_scope_to_consent,{RPId,Scope,Consent}}).


set_rp_pass(RPId,Pass) -> gen_server:call(?MODULE,{set_rp_pass,{RPId,Pass}}).

validate_code(ClientId,Code,RedirectUri) -> gen_server:call(?MODULE,{validate_code,{ClientId,Code,RedirectUri}}).
authorize(ClientId,RedirectUri,UserId) -> gen_server:call(?MODULE,{authorize,{ClientId,RedirectUri,UserId}}).

get_userinfo(Token) -> idp_usermng:get_userviatoken(Token).

reg_user(Username,Password) -> idp_usermng:reg_user(Username,Password). 
validate_user(Username,Password) -> idp_usermng:verify(Username,Password).


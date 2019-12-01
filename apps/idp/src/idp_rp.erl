-module(idp_rp).
-behaviour(gen_server).
-export([
         start_link/1,
         authorize/1,
         add_scope_to_consent/2,
         remove_scope_to_consent/2,
         validate_code/2,
         clean_token/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {id = undefined,codes=[],password=undefined,consents=undefined,code_size=32}).
-record(token, {access_token,token_type,expires_in,expired=true,refresh_token,scopes,created_time}).
-define(SERVER, ?MODULE).
-define(CODE_SIZE, 32).
-define(TOKEN_SIZE, 256).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
    Pass = binary_to_list(base64:encode(crypto:strong_rand_bytes(10))),
    Consent = ets:new(consent,[set]),
    ets:insert(Consent,{unconfirmed,[]}),
    {ok, #state{id = Id,password = Pass,consents = Consent}}.

handle_call({set_rp_pass,Pass}, _From, State) ->
    NewState = State#state{password=Pass},
    Reply={ok,Pass},
    {reply, Reply, NewState};

handle_call({set_code_size,NewCodeSize}, _From, #state{password = RP_Pass} = State) ->
    %Not caring for access control for RP yet
    %{Reply,NewState} = case RP_Pass == Pass of 
    {Reply,NewState} = case true of 
        true -> {ok,State#state{code_size=NewCodeSize}};
        false -> {{error,bad_password},State}
    end,
    {reply, Reply, NewState};

handle_call({get_pwid}, _From, #state{id = Id, password = RP_Pass} = State) ->
    Reply = {ok,{Id,RP_Pass}},
    {reply, Reply, State};


handle_call({get_consent,Consent}, _From,  #state{consents = Consents} = State) ->
    Reply = case ets:lookup(Consents,Consent) of 
                [{Consent,Scopes}] ->
                    {ok,{Consent,Scopes}};
                _ -> 
                    {error,consent_not_found}
            end,
    {reply, Reply, State};

handle_call({get_consents}, _From, #state{consents = Consents} = State) ->
    Reply = {ok,ets_keys(Consents,ets:first(Consents),[])},
    {reply, Reply, State};

handle_call({validate_rp,Password}, _From, #state{password = RP_Pass} = State) ->
    Reply = {ok, Password == RP_Pass},
    {reply, Reply, State};

handle_call({change_pass,OldPass,NewPass}, _From, #state{password = RP_Pass} = State) ->
    {Reply,NewState} = case OldPass == RP_Pass of
                           true ->
                               {ok,State#state{password = NewPass}};
                           false ->
                               {error,State}
                       end,
    {reply, Reply, NewState};

handle_call({expire_token,AccessToken}, _From,  State) ->
    Reply = idp_usermng:expire_token(AccessToken),
    {reply, Reply, State};

handle_call({add_consent,Consent}, _From,  #state{consents = Consents} = State) ->
    Reply = case ets:lookup(Consents,Consent) of 
       [] ->
            case ets:insert(Consents,{Consent,[]}) of
                true -> 
                    {ok};
                _ -> 
                    {error,unable_to_insert}
            end;
        _ -> 
            {error,consent_exists}
    end,
    {reply, Reply, State};


handle_call({remove_consent,Consent}, _From,  #state{consents = Consents} = State) ->
    Reply = case ets:lookup(Consents,Consent) of 
       [{Consent,_}] ->
            case ets:delete(Consents,Consent) of
                true -> 
                    {ok};
                _ -> 
                    {error,unable_to_remove}
            end;
        _ -> 
            {error,consent_not_exists}
    end,
    {reply, Reply, State};


handle_call({add_scope,Scope}, _From,  #state{consents = Consents} = State) ->
    Reply = case ets:lookup(Consents,unconfirmed) of 
                [{Concent,CScopes}] ->
                    case lists:member(Scope,CScopes) of 
                        false -> 
                            ets:insert(Consents,{Concent,[Scope|CScopes]}),
                            {ok};
                        _ ->
                            {error,scope_already_bound}
                    end;
                _ -> {error,no_such_consent}
    end,
    {reply, Reply, State};

handle_call({remove_scope,Scope}, _From,  #state{consents = Consents} = State) ->
    Reply = case ets:lookup(Consents,unconfirmed) of 
                [{Concent,CScopes}] ->
                    case lists:member(Scope,CScopes) of 
                        true -> 
                            ets:insert(Consents,{Concent,lists:delete(Scope,CScopes)}),
                            {ok};
                        _ ->
                            {error,scope_already_bound}
                    end;
                _ -> {error,no_such_consent}
    end,
    {reply, Reply, State};


handle_call({add_scope_to_consent,{Consent,Scope}}, _From,  #state{consents = Consents} = State) ->
    Reply = case ets:lookup(Consents,Consent) of 
                [{Concent,CScopes}] ->
                    case lists:member(Scope,CScopes) of 
                        false -> 
                            ets:insert(Consents,{Concent,[Scope|CScopes]}),
                            {ok};
                        _ ->
                            {error,scope_already_bound}
                    end;
                _ -> {error,no_such_consent}
    end,
    {reply, Reply, State};


handle_call({remove_scope_to_consent,{Consent,Scope}}, _From,  #state{consents = Consents} = State) ->
    Reply = case ets:lookup(Consents,Consent) of 
                [{Concent,CScopes}] ->
                    case lists:member(Scope,CScopes) of 
                        true -> 
                            ets:insert(Consents,{Concent,lists:delete(Scope,CScopes)}),
                            {ok};
                        _ ->
                            {error,scope_already_bound}
                    end;
                _ -> {error,no_such_consent}
    end,
    {reply, Reply, State};


handle_call({authorize,RedirectUri,UserID}, _From, #state{code_size = CodeSize, codes = Codes} = State) ->
    Code = create_code(CodeSize),
    Reply = {ok, Code},
    NewState = State#state{codes=[{Code,{RedirectUri,UserID}}|Codes]},
    {reply, Reply, NewState};

handle_call({validate_code,{Code,RedirectUri}}, _From, #state{codes = Codes} = State) ->
    logger:debug("Validate_code: ~p",[Code]),
    {Reply,NewState} = case lists:keyfind(Code,1,Codes) of
                           {Code,{FRedirectUri,User}} ->
                               logger:debug("RedirectUri: ~p~nFRedirectUri: ~p~nCode: ~p~n",[RedirectUri,FRedirectUri,Code]),
                               case (FRedirectUri == RedirectUri) or (FRedirectUri == undefined)  of
                                   true ->
                                       NewCodes = lists:keydelete(Code,1,Codes),
                                       AccessToken = create_code(?TOKEN_SIZE),
                                       RefreshToken = create_code(?TOKEN_SIZE),
                                       TokenType = "Bearer",
                                       ExpiresIn= "3600",
                                       Token = #token{access_token=AccessToken,token_type=TokenType,expires_in=ExpiresIn,expired=false,refresh_token=RefreshToken,created_time=erlang:timestamp()},
                                       idp_usermng:set_token(User,Token),
                                       erlang:spawn(?MODULE,clean_token,[erlang:self(),AccessToken,list_to_integer(ExpiresIn)]),
                                       RetToken = #{<<"access_token">> => list_to_binary(AccessToken), 
                                                    <<"token_type">> => list_to_binary(TokenType),
                                                    <<"expires_in">> => list_to_binary(ExpiresIn),
                                                    <<"refresh_token">> =>list_to_binary(RefreshToken)},

                                       {{ok,RetToken},State#state{codes=NewCodes}};
                                   _ ->
                                       {{error, bad_redirecturi},State}
                               end;
                           _R ->
                               {{error, bad_code},State}
                       end,
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

clean_token(From,AccessToken,SecTime) ->
    timer:sleep(SecTime*1000),
    gen_server:call(From,{expire_token,AccessToken}).

create_code(Size) ->
    BinToken = crypto:strong_rand_bytes(Size),
    [case X of
         43 -> 45;
         47 -> 95;
         L -> L
     end || X<- base64:encode_to_string(BinToken)].

%% Internal functions
%%
validate_code(ClientId,Code) -> gen_server:call(?MODULE,{validate_code,{ClientId,Code}}).
authorize(ClientId) -> gen_server:call(?MODULE,{authorize,ClientId}).
add_scope_to_consent(Scope,Consent) -> gen_server:call(?MODULE,{add_scope_to_consent,{Scope,Consent}}).
remove_scope_to_consent(Scope,Consent) -> gen_server:call(?MODULE,{remove_scope_to_consent,{Scope,Consent}}).

ets_keys(_Table,'$end_of_table',Acc) -> Acc;
ets_keys(Table,First,Acc) -> ets_keys(Table,ets:next(Table,First),[First|Acc]).


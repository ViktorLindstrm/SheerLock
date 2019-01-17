%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(authorize_handler).

-export([init/2]).
%5 min session time
-define(SESSION_TIME, 5*60).

-record(user, {id,
               session          :: list(),
               username         :: atom(),
               name             :: list(),
               password         :: list(),
               scopes = []      :: list(),
               token = undefined,
               code = undefined :: 'undefined' | list()
              }).

init(Req0, Opts) ->

    Method = cowboy_req:method(Req0),
    method(Method,Req0,Opts).

method(<<"POST">>,Req0,Opts)->
    logger:debug("Auth POST"),
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    Username = proplists:get_value(<<"username">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),
    RedirectUri = proplists:get_value(<<"redirecturi">>, PostVals),
    ClientId = proplists:get_value(<<"client_id">>, PostVals),
    BinClientId = erlang:binary_to_atom(ClientId,utf8),
    Scopes = proplists:get_value(<<"scope">>, PostVals),
    BinScopes = erlang:binary_to_atom(Scopes,utf8),
    State = proplists:get_value(<<"state">>, PostVals),
    BinUser = binary_to_atom(Username,utf8),
    BinPassword = binary_to_list(Password),

    Req1 = case idp_mng:validate_user(BinUser,BinPassword) of
               {ok,true} ->
                   {ok,UserScopes} = idp_usermng:get_scopes(BinUser),
                   {ok,ScopeConsents} = idp_mng:get_consents(BinClientId),
                   RPScopes = lists:flatten([get_scopes_from_consent(BinClientId,Consent) || Consent <- ScopeConsents]),
                   case is_in_list(BinScopes,RPScopes) of 
                       [] -> 
                           case is_in_list(BinScopes,UserScopes) of
                               [] ->  
                                   logger:debug("Scopes: ~p, UserConsent: ~p, RP scopes: ~p~n",[Scopes,UserScopes,RPScopes]),
                                   {ok,Code} = idp_mng:authorize(BinClientId,RedirectUri,BinUser),
                                   BinCode = erlang:list_to_binary(Code),
                                   Response = <<RedirectUri/binary,<<"?code=">>/binary,BinCode/binary,<<"&state=">>/binary,State/binary>>,
                                   {ok,SessionID} = idp_usermng:set_session(BinUser,?SESSION_TIME),
                                   Req2 = cowboy_req:set_resp_cookie(<<"session">>, SessionID, Req, #{port => 8180,  http_only => true}),
                                   _Req3 = cowboy_req:reply(302, #{<<"Location">> => Response}, <<>>, Req2);
                               _ ->
                                   logger:debug("adding missing scopes"),

                                   MissingScopes = is_in_list(BinScopes,UserScopes),
                                   [idp_usermng:add_scope(BinUser,S) || S <- MissingScopes],
                                   logger:debug("Added User scopes: ~p~n",[MissingScopes])
                           end;
                       R -> 
                           logger:debug("Scopes not allowed for RP/Clilent: ~p~n",[R])
                   end;
               {ok,false} ->
                   logger:debug("Bad password"),
                   cowboy_req:reply(405, #{}, <<>>, Req0);
               {error,no_such_user} ->
                   logger:debug("No such user"),
                   cowboy_req:reply(405, #{}, <<>>, Req0);
               Default ->
                   logger:error("Error! ~p~n",[Default])
           end,
    {ok, Req1, Opts};


method(<<"GET">>,Req0,Opts) ->
    #{response_type := ResponseType,
      client_id := ClientId,
      redirect_uri := RedirectUri,
      scope := Scope,
      state := State} = cowboy_req:match_qs([{response_type, [], undefined},
                                             {client_id,     [], undefined},
                                             {redirect_uri,  [], undefined},
                                             {scope,         [], ""},
                                             {state,         [], ""}
                                            ], Req0),

    Req = authorize(<<"GET">>, {ResponseType,ClientId,RedirectUri,Scope,State}, Req0),
    {ok, Req, Opts}.

authorize(<<"GET">>, {undefined,undefined,undefined,undefined,undefined}, Req) ->
    cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/html">>
     }, <<"<html><body>
        <a href=\"http://127.0.0.1:8180/authorize\?response_type\=code\&client_id\=test\&state\=xyz\&redirect_uri\=http://127.0.0.1:8180/\">send</a>
     </body></html>">>, Req);


authorize(<<"GET">>, {<<"code">>,ClientId,RedirectUri,Scope,State}, Req) ->
    logger:debug("Auth GET"),
    Cookies = cowboy_req:parse_cookies(Req),
    Ret = case lists:keyfind(<<"session">>, 1,Cookies) of 
              {_,SID} -> 
                  logger:debug("Found session: ~p",[SID]),
                  BinClientId = binary_to_atom(ClientId,utf8),
                  BinScopes = binary_to_atom(Scope,utf8),
                  {ok,UserPid} = idp_usermng:get_userviasession(binary_to_list(SID)),
                  logger:debug("Get userviasession: UserPID: ~p",[UserPid]),
                  {ok,UserData} = gen_server:call(UserPid,{get_user}),
                  BinUser = UserData#user.username,
                  UserScopes= UserData#user.scopes,
                  {ok,ScopeConsents} = idp_mng:get_consents(BinClientId),
                  RPScopes = lists:flatten([get_scopes_from_consent(BinClientId,Consent) || Consent <- ScopeConsents]),
                  case is_in_list(BinScopes,RPScopes) of 
                      [] -> 
                          case is_in_list(BinScopes,UserScopes) of
                              [] ->  
                                  logger:debug("Scopes: ~p, UserConsent: ~p, RP scopes: ~p~n",[Scope,UserScopes,RPScopes]),
                                  {ok,Code} = idp_mng:authorize(BinClientId,RedirectUri,BinUser),
                                  BinCode = erlang:list_to_binary(Code),
                                  Response = <<RedirectUri/binary,<<"?code=">>/binary,BinCode/binary,<<"&state=">>/binary,State/binary>>,
                                  {ok,SessionID} = idp_usermng:set_session(BinUser,?SESSION_TIME),
                                  Req2 = cowboy_req:set_resp_cookie(<<"session">>, SessionID, Req,#{http_only => true}),
                                  cowboy_req:reply(302, #{<<"Location">> => Response}, <<>>, Req2);
                              _ ->
                                  MissingScopes = is_in_list(BinScopes,UserScopes),
                                  [idp_usermng:add_scope(BinUser,S) || S <- MissingScopes],
                                  logger:debug("Added User scopes: ~p~n",[MissingScopes])
                          end;
                      R -> 
                          logger:debug("Scopes not allowed for RP/Clilent: ~p~n",[R])
                  end;
              false -> 
                  logger:debug("No session yet"),
                  Page = [<<"<html><head>
                            <link rel=\"stylesheet\" type=\"text/css\" href=\"priv/login.css\"></head>
                            <body>
                                <div id=\"header\">
                                <h1>Login</h1>
                                </div>
                                <div id=\"login\">   
                                    <form action=\"/scopes\" method=\"post\">
                                      <p>Username:</p> <input type=\"text\" name=\"username\"><br>
                                      <p>Password:</p> <input type=\"text\" name=\"password\"><br>
                                      <input type=\"hidden\" name=\"redirecturi\" value=\"">>,RedirectUri,<<"\"><br>
                                      <input type=\"hidden\" name=\"client_id\" value=\"">>,ClientId,<<"\"><br>
                                      <input type=\"hidden\" name=\"state\" value=\"">>,State,<<"\"><br>
                                      <input type=\"hidden\" name=\"scope\" value=\"">>,Scope,<<"\"><br>
                                      <input type=\"submit\" value=\"Submit\">
                                  </form>
                                 </body></html>">>],
                  cowboy_req:reply(200, #{
                    <<"content-type">> => <<"text/html">>
                   }, Page, Req) 
          end,
    Ret;

authorize(<<"GET">>, {_,_,_,_,_}, Req) ->
    cowboy_req:reply(400, #{}, <<"Missing echo parameter, is? 4">>, Req);

authorize(Method, Message, Req) ->
    logger:error("Error! Method not allowed: ~p, Message: ~p, Req: ~p~n",[Method, Message, Req]),
    cowboy_req:reply(405, Req).

get_scopes_from_consent(RPName,Consent) ->
    {ok,{_,Scopes}} = idp_mng:get_consent(RPName,Consent),
    Scopes.

is_in_list(Checks,Checkee) ->
    ResList = lists:flatten([X || X <- [Checks], not(lists:member(X,Checkee))]),
    logger:debug("Res: ~p, Checks: ~p, Checkee: ~p~n",[ResList,Checks,Checkee]),
    ResList.

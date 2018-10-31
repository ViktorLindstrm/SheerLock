%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(authorize_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    method(Method,Req0,Opts).

method(<<"POST">>,Req0,Opts)->
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
                  {ok,UserScopes} = idp_user:get_scopes(BinUser),
                  {ok,ScopeConsents} = idp_mng:get_consents(BinClientId),
                  RPScopes = lists:flatten([get_scopes_from_consent(BinClientId,Consent) || Consent <- ScopeConsents]),
                  case is_in_list(BinScopes,RPScopes) of 
                      [] -> 
                          case is_in_list(BinScopes,UserScopes) of
                              [] ->  
                                  logger:debug("Scopes: ~p, UserConsent: ~p, RP scopes: ~p~n",[Scopes,UserScopes,RPScopes]),
                                  {ok,Code} = idp_mng:authorize(BinClientId,RedirectUri),
                                  BinCode = erlang:list_to_binary(Code),
                                  Response = <<RedirectUri/binary,<<"?code=">>/binary,BinCode/binary,<<"&state=">>/binary,State/binary>>,
                                  _SetCode = idp_user:set_code(binary_to_atom(Username,utf8),Code),
                                  cowboy_req:reply(302, #{<<"Location">> => Response}, <<>>, Req);
                              _ ->
                                  MissingScopes = is_in_list(BinScopes,UserScopes),
                                  [idp_user:add_scope(BinUser,S) || S <- MissingScopes],
                                  logger:debug("Added User scopes: ~p~n",[MissingScopes])
                          end;
                          R -> io:format("Scopes not allowed for RP/Clilent: ~p~n",[R])
                  end;
              {ok,false} ->
                  io:format("Bad password"),
                  cowboy_req:reply(405, #{}, <<>>, Req0);
              {error,no_such_user} ->
                  io:format("No such user"),
                  cowboy_req:reply(405, #{}, <<>>, Req0)
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
    Page = [<<"<html><body>
                  <form action=\"/authorize\" method=\"post\">
                      Username: <input type=\"text\" name=\"username\"><br>
                      Password: <input type=\"text\" name=\"password\"><br>
                      <input type=\"hidden\" name=\"redirecturi\" value=\"">>,RedirectUri,<<"\"><br>
                      <input type=\"hidden\" name=\"client_id\" value=\"">>,ClientId,<<"\"><br>
                      <input type=\"hidden\" name=\"state\" value=\"">>,State,<<"\"><br>
                      <input type=\"hidden\" name=\"scope\" value=\"">>,Scope,<<"\"><br>
                      <input type=\"submit\" value=\"Submit\">
                  </form>
                 </body></html>">>],
    cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/html">>
     }, Page, Req);

authorize(<<"GET">>, {_,_,_,_,_}, Req) ->
    cowboy_req:reply(400, #{}, <<"Missing echo parameter, is? 4">>, Req);

authorize(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

get_scopes_from_consent(RPName,Consent) ->
    {ok,{_,Scopes}} = idp_mng:get_consent(RPName,Consent),
    Scopes.

is_in_list(Checks,Checkee) ->
    ResList = lists:flatten([X || X <- [Checks], not(lists:member(X,Checkee))]),
    logger:debug("Res: ~p, Checks: ~p, Checkee: ~p~n",[ResList,Checks,Checkee]),
    ResList.
   %[X || X <- Checks, not(lists:member(X,Checkee))]



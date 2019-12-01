-module(scopes_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    logger:debug("Scopes method: ~p~nReq~p",[Method,Req0]),
    method(Method,Req0,Opts).

%%Display scopes for user
method(<<"GET">>,Req0,Opts)->
    Cookies = cowboy_req:parse_cookies(Req0),
    Req = case lists:keyfind(<<"session">>, 1,Cookies) of
              {_,SID} ->
                  {ok,Pid} = idp_mng:get_userviasession(SID),
                  logger:debug("User Session: ~p",[Pid]),
                  {ok,UserScopes} = gen_server:call(Pid,{get_scopes}),
                  Scopes = UserScopes,
                  LScopes = ["<td>"++S++"</td>" ||S <- Scopes],
                  logger:debug("User Scopes: ~p",[UserScopes]),
                  cowboy_req:reply(200, #{
                    <<"content-type">> => <<"text/html">>
                   }, [<<"<html><body><table>">>,"<th>Scopes</th>",
                       LScopes,"</table>","</br>",
                       <<"</body></html>">>], Req0);
              _ ->
                  logger:debug("Cant fint user from session"),
                  cowboy_req:reply(405, Req0)
          end,
    {ok, Req, Opts};


%Display scopes and show what is missing from RP (Posted in)
method(<<"POST">>,Req0,Opts)->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    RedirectUri = proplists:get_value(<<"redirecturi">>, PostVals),
    State = proplists:get_value(<<"state">>, PostVals),
    Scope = proplists:get_value(<<"scope">>, PostVals),
    ClientId = proplists:get_value(<<"client_id">>, PostVals),
    BinClientId = binary_to_atom(ClientId,utf8),
    Username = proplists:get_value(<<"username">>, PostVals),
    BinUser = binary_to_atom(Username,utf8),
    Password = proplists:get_value(<<"password">>, PostVals),
    BinPassword = binary_to_list(Password),

    {ok,ScopeConsents} = idp_mng:get_consents(BinClientId),
    RPScopes = lists:flatten([get_scopes_from_consent(BinClientId,Consent) || Consent <- ScopeConsents]),
    logger:debug("RPScopes: ~p, ScopeConsents: ~p, BinClientId: ~p",[RPScopes, ScopeConsents, BinClientId]),
    Cookies = cowboy_req:parse_cookies(Req),

    Req2 = case lists:keyfind(<<"session">>, 1,Cookies) of
               {_,SID} ->
                   {ok,Pid} = idp_mng:get_userviasession(SID),
                   logger:debug("User Session: ~p",[Pid]),
                   {ok,UserScopes} = gen_server:call(Pid,{get_scopes}),
                   Scopes = RPScopes--UserScopes,
                   AcceptedScopes = ["<td>"++atom_to_list(S)++"</td>" || S <- UserScopes],
                   LScopes = ["<td>"++atom_to_list(S)++"</td>" ||S <- Scopes],
                   logger:debug("User Scopes: ~p",[UserScopes]),
                   cowboy_req:reply(200, #{
                     <<"content-type">> => <<"text/html">>
                    }, [<<"<html><body><table>">>,"<th>Accepted Scopes</th>",
                        AcceptedScopes,"<th>To be accepted</th>",
                        LScopes,"</table>","</br>",
                        <<"</body></html>">>], Req);
               _ ->
                   case idp_mng:validate_user(BinUser,BinPassword) of
                       {ok,true} ->
                           {ok,UserScopes} = idp_usermng:get_scopes(BinUser),
                           Scopes = RPScopes--UserScopes,
                           AcceptedScopes = ["<td>"++atom_to_list(S)++"</td>" || S <- UserScopes],

                           LScopes = ["<tr><td>"++atom_to_list(S)++"</td></tr>" ||S <- Scopes],
                           Accept = ["<form action=\"/authorize\" method=\"post\">",
                                      <<"<input type=\"hidden\" name=\"username\" value=\"">>,Username,<<"\"><br>">>,
                                      <<"<input type=\"hidden\" name=\"password\" value=\"">>,Password,<<"\"><br>">>,
                                      <<"<input type=\"hidden\" name=\"redirecturi\" value=\"">>,RedirectUri,<<"\"><br>">>,
                                      <<"<input type=\"hidden\" name=\"client_id\" value=\"">>,ClientId,<<"\"><br>">>,
                                      <<"<input type=\"hidden\" name=\"state\" value=\"">>,State,<<"\"><br>">>,
                                      <<"<input type=\"hidden\" name=\"scope\" value=\"">>,Scope,<<"\"><br>">>,
                                      <<"<input type=\"submit\" value=\"Accept scopes\">">>,
                                      "</form>"],

                           cowboy_req:reply(200, #{
                             <<"content-type">> => <<"text/html">>
                            }, [<<"<html><body><table>">>,"<th>Accepted Scopes</th>",
                                AcceptedScopes,"<th>To be accepted</th>",
                                LScopes,"</table>","</br>",Accept,
                                <<"</body></html>">>], Req);
                       _ ->
                           logger:debug("Cant fint user"),
                           cowboy_req:reply(405, Req)
                   end

           end,

    logger:debug("puff"),
    %ReqX = cowboy_req:reply(200, #{
             %<<"content-type">> => <<"text/html">>
            %}, [<<"<html><body><table>">>], Req2),
    {ok, Req2, Opts}.

%extract_code(RP) ->
    %{ok,{_,Code}} = idp_mng:get_rp(RP),
    %Code.
get_scopes_from_consent(RPName,Consent) ->
    {ok,{_,Scopes}} = idp_mng:get_consent(RPName,Consent),
    Scopes.

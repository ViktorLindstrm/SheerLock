%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(register_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    method(Method,Req0,Opts).

method(<<"POST">>, Req0, Opts)->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    Username = proplists:get_value(<<"username">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),
    ResponseType = proplists:get_value(<<"responsetype">>, PostVals),
    RedirectUri = proplists:get_value(<<"redirecturi">>, PostVals),
    ClientId = proplists:get_value(<<"client_id">>, PostVals),
    Scopes = proplists:get_value(<<"scope">>, PostVals),
    State = proplists:get_value(<<"state">>, PostVals),

    logger:debug("u:p - ~p,~p~n~p",[Username,Password,PostVals]),
    Req1 = case idp_mng:reg_user(binary_to_list(Username),binary_to_list(Password)) of
              ok ->
                   io:format("hejsan"),
                  cowboy_req:reply(302, #{<<"Location">> => <<"/authorize?response_type=",ResponseType/binary,"&redirect_uri=",RedirectUri/binary,"&client_id=",ClientId/binary,"&scope=",Scopes/binary,"&state=",State/binary>>}, <<>>, Req);
              {error,no_such_user} ->
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
                                             {scope,         [], undefined},
                                             {state,         [], undefined}
                                            ], Req0),

    logger:debug("reg - ~p",[ClientId]),
    Req = authorize(<<"GET">>,ResponseType,ClientId,RedirectUri,Scope,State, Req0),
    {ok, Req, Opts}.



authorize(<<"GET">>,undefined,undefined,undefined,undefined,undefined, Req) ->
    Page = [<<"<html><body>
              <h1>Register</h1>
                  <form action=\"/register\" method=\"post\">
                      Username: <input type=\"text\" name=\"username\"><br>
                      Password: <input type=\"text\" name=\"password\"><br>
                      <input type=\"submit\" value=\"Submit\">
                  </form>
                 </body></html>">>],
    cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/html">>
     }, Page, Req);

authorize(<<"GET">>,ResponseType,ClientId,RedirectUri,Scope,State, Req) -> 
    logger:debug("Posting data"),
    Page = [<<"<html><body>
              <h1>Register</h1>
                  <form action=\"/register\" method=\"post\">
                      Username: <input type=\"text\" name=\"username\"><br>
                      Password: <input type=\"text\" name=\"password\"><br>
                      <input type=\"hidden\" name=\"responsetype\" value=\"">>,ResponseType,<<"\"><br>
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


authorize(_,_,_,_,_,_,Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

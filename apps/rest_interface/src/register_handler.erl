%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(register_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    method(Method,Req0,Opts).

method(<<"POST">>, Req0, Opts)->
    io:format("Hejsna"),
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    Username = proplists:get_value(<<"username">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),

    Req1 = case idp_mng:reg_user(binary_to_list(Username),binary_to_list(Password)) of
              ok ->
                   io:format("hejsan"),
                  cowboy_req:reply(302, #{<<"Location">> => <<"/authorize">>}, <<>>, Req);
              {error,no_such_user} ->
                  cowboy_req:reply(405, #{}, <<>>, Req0)
          end,
    {ok, Req1, Opts};


method(<<"GET">>,Req0,Opts) ->
    io:format("GETHEJ"),
    Req = authorize(<<"GET">>, Req0),
    {ok, Req, Opts}.

%authorize(<<"GET">>, Req) ->
    %cowboy_req:reply(200, #{
      %<<"content-type">> => <<"text/html">>
     %}, <<"<html><body>
        %<a href=\"http://127.0.0.1:8180/authorize\?response_type\=code\&client_id\=test\&state\=xyz\&redirect_uri\=http://127.0.0.1:8180/\">send</a>
     %</body></html>">>, Req);


authorize(<<"GET">>, Req) ->
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

%authorize(<<"GET">>, Req) ->
    %cowboy_req:reply(400, #{}, <<"Missing echo parameter, is? 4">>, Req);

authorize(_, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

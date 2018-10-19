%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(token_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),

    Req = case cowboy_req:parse_header(<<"authorization">>, Req0) of
              {basic, User, Pass } ->
                  Validate = idp_mng:validate_rp(erlang:binary_to_atom(User,utf8),binary_to_list(Pass)),
                  case Validate of
                      {ok,true} ->
                          {ok, PostVals, Req1} = cowboy_req:read_urlencoded_body(Req0),
                          GrantType = proplists:get_value(<<"grant_type">>, PostVals),
                          Code = binary_to_list(proplists:get_value(<<"code">>, PostVals)),
                          RedirectUri = proplists:get_value(<<"redirect_uri">>, PostVals),
                          ClientId = binary_to_atom(proplists:get_value(<<"client_id">>, PostVals),utf8),

                          token(Method, {GrantType,Code,RedirectUri,ClientId}, Req1);
                      _ ->
                          cowboy_req:reply(401, #{}, <<>>, Req0)
                  end;
              _ ->
                  cowboy_req:reply(401, #{}, <<>>, Req0)
          end,
    {ok, Req, Opts}.



token(<<"POST">>, {<<"authorization_code">>,Code,RedirectUri,ClientId}, Req) ->
    Val = idp_mng:validate_code(ClientId,Code,RedirectUri),
    case Val of
        {ok, Token}  ->
            Response = jiffy:encode(Token),
            cowboy_req:reply(200, #{
              <<"content-type">> => <<"application/json; charset=utf-8">>
             },Response, Req);
        E ->
            io:format("CodeVal Error: ~p~n",[E]),
            cowboy_req:reply(405, Req)
    end;

token(<<"POST">>, {_,_,_,_,_}, Req) ->
    cowboy_req:reply(400, #{}, <<"Missing parameter">>, Req);

token(_, _, Req) ->
    %io:format("hejsan~n"),
    logger:error("Method not allowed"),
    %% Method not allowed.
    cowboy_req:reply(405, Req).



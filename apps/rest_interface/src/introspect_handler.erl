%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(introspect_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),

    Req = case cowboy_req:parse_header(<<"authorization">>, Req0) of
              {basic, User, Pass } ->
                  Validate = idp_mng:validate_rp(erlang:binary_to_atom(User,utf8),binary_to_list(Pass)),
                  case Validate of
                      {ok,true} ->
                          {ok, PostVals, Req1} = cowboy_req:read_urlencoded_body(Req0),
                          Token = proplists:get_value(<<"token">>, PostVals),
                          Response = <<"hello">>,
                          cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json; charset=utf-8">>
                           },Response, Req1);
                      _ ->
                          cowboy_req:reply(401, #{}, <<>>, Req0)
                  end;
              _ ->
                  cowboy_req:reply(401, #{}, <<>>, Req0)
          end,
    {ok, Req, Opts}.



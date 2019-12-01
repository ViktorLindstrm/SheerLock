%% Feel free to use, reuse and abuse the code in this file.
%% @doc GET echo handler.
-module(admin_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    method(Method,Req0,Opts).

method(<<"GET">>,Req0,Opts)->
    {ok,RPs} = idp_mng:get_rps(),
    RPList  = [layout:client(atom_to_list(RP),atom_to_list(RP),extract_code(RP)) || RP <- RPs],
    List = ["<div class=\"card-columns\">",RPList,"</div>"],
    Reg = "<form action=\"/admin\" method=\"post\">Register Relying Party<br>Name:<input type=\"text\" name=\"rp_name\"> <input type=\"submit\" value=\"Submit\"> </form>",

    Content = [<<"<table>">>,List,"</table>","</br>", Reg],

    LayoutContent = layout:content(Content),
    Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/html">>
     }, [layout:header("Admin"),LayoutContent], Req0),
    {ok, Req, Opts};


method(<<"POST">>,Req0,Opts)->

    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    BinRPName = proplists:get_value(<<"rp_name">>, PostVals),
    BinRPDel = proplists:get_value(<<"rp_del">>, PostVals),
    BinRPConf = proplists:get_value(<<"rp_conf">>, PostVals),

    case BinRPName of 
        undefined -> undefined;
        BinRPName -> 
            RPName = binary_to_atom(BinRPName,utf8),
            idp_mng:register_rp(RPName)
    end,
    Req1 = case BinRPConf of 
        undefined -> 
            cowboy_req:reply(302, #{
              <<"location">> => <<"/admin">>
             }, [], Req);
        BinRPConf -> 
            cowboy_req:reply(302, #{
              <<"location">> => <<"/admin/",BinRPConf/binary>>
             }, [], Req)
    end,
    case BinRPDel of 
        undefined -> undefined;
        BinRPDel -> 
            RPDel = binary_to_atom(BinRPDel ,utf8),
            idp_mng:unregister_rp(RPDel)
    end,

    {ok, Req1, Opts}.


extract_code(RP) ->
    {ok,{_,Code}} = idp_mng:get_rp(RP),
    Code.

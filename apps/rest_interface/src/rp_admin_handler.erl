%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(rp_admin_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    method(Method,Req0,Opts).


method(<<"POST">>,Req0,Opts) ->
    RPNameBin = cowboy_req:binding(rp,Req0),
    RPName = binary_to_atom(RPNameBin,utf8),
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    NewScope = binary_to_atom(proplists:get_value(<<"new_scope">>, PostVals),utf8),

    idp_mng:add_scope(RPName,NewScope),

    Req1 = cowboy_req:reply(302, #{
      <<"location">> => <<"/admin/",RPNameBin/binary>>
     }, [], Req),
    {ok, Req1, Opts};



method(<<"GET">>,Req0,Opts) ->
    RPName = binary_to_atom(cowboy_req:binding(rp,Req0),utf8),
    {ok,Consents} = idp_mng:get_consents(RPName),

    CList = [["<li>"++atom_to_list(Consent)++"</li><ul>",["<li>"++atom_to_list(CScope)++"</li>"|| CScope <- get_scopes_from_consent(RPName,Consent)],"</ul>"] || Consent <- Consents],
    EList = ["<ul>",[X || X <- get_scopes_from_consent(RPName,unconfirmed)],"</ul>"],
    List = ["<ul>",CList,"</ul>"],
    Reg = "<form  method=\"post\">Add new scope<br>Name:<input type=\"text\" name=\"new_scope\"> <input type=\"submit\" value=\"Submit\"> </form>",

    Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/html">>
     }, [<<"<html><body>">>, List,Reg,
     <<"</body></html>">>], Req0),
    {ok, Req, Opts}.


get_scopes_from_consent(RPName,Consent) ->
    {ok,{_,Scopes}} = idp_mng:get_consent(RPName,Consent),
    Scopes.

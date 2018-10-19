%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(rp_admin_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    method(Method,Req0,Opts).


method(<<"GET">>,Req0,Opts) ->
    RPName = binary_to_atom(cowboy_req:binding(rp,Req0),utf8),
    {ok,Consents} = idp_mng:get_consents(RPName),
    %{ok,Scopes}   = idp_mng:get_scopes(RPName),

    CList = [["<li>"++atom_to_list(Consent)++"</li><ul>",["<li>"++atom_to_list(CScope)++"</li>"|| CScope <- get_scopes_from_consent(RPName,Consent)],"</ul>"] || Consent <- Consents],
    EList = ["<ul>",[X || X <- get_scopes_from_consent(RPName,unconfirmed)],"</ul>"],
    List = ["<ul>",CList,"</ul>"],
    %Reg = "<form action=\"/admin\" method=\"get\">Add Consent:<br><input type=\"text\" name=\"consent\"> <input type=\"submit\" value=\"Submit\"> </form>",

    Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/html">>
     }, [<<"<html><body>">>, List,
     <<"</body></html>">>], Req0),
    {ok, Req, Opts}.


get_scopes_from_consent(RPName,Consent) ->
    {ok,{_,Scopes}} = idp_mng:get_consent(RPName,Consent),
    Scopes.

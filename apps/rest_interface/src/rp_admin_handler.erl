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
    case proplists:get_value(<<"new_scope">>, PostVals) of 
        undefined ->
            case binary_to_integer(proplists:get_value(<<"new_code_size">>, PostVals)) of 
                NewCodeSize -> 
                    logger:debug("new CodeSize: ~p",[NewCodeSize]),
                    idp_mng:set_rp_code_size(RPName,NewCodeSize);
                _ ->
                    logger:debug("Error! not able to parse NewCodeSize")
            end;
        NewScopeAtom -> 
            NewScope = binary_to_atom(NewScopeAtom,utf8),
            idp_mng:add_scope(RPName,NewScope)
    end,

    Req1 = cowboy_req:reply(302, #{
      <<"location">> => <<"/admin/",RPNameBin/binary>>
     }, [], Req),
    {ok, Req1, Opts};


method(<<"GET">>,Req0,Opts) ->
    RPName = binary_to_atom(cowboy_req:binding(rp,Req0),utf8),
    {ok,Consents} = idp_mng:get_consents(RPName),

    CList = [["<li class=\"list-group-item\">"++atom_to_list(Consent)++"</li><ul>",["<li class=\"list-group-item\">"++atom_to_list(CScope)++"</li>"|| CScope <- get_scopes_from_consent(RPName,Consent)],"</ul>"] || Consent <- Consents],
    List = ["<h2>Scopes allowed</h2>","<ul class=\"list-group\">",CList,"</ul>"],
    {ok,CodeSize} = idp_mng:get_rp_code_size(RPName),
    PrintCodeSize = integer_to_list(CodeSize),
    Reg = 
    ["<form method=\"post\">
      <div class=\"form-group row\">
        <label for=\"addScope\" class=\"col-sm-2 col-form-label\">Add scope</label>
        <div class=\"col-sm-10\">
          <input type=\"text\" name=\"new_scope\" class=\"form-control\" id=\"addScope\" >  
        </div>
      </div>
      </form>",
      "<form method=\"post\">
      <div class=\"form-group row\">
        <label for=\"codeSize\" class=\"col-sm-2 col-form-label\">Code size</label>
        <div class=\"col-sm-10\">
          <input type=\"number\" name=\"new_code_size\" class=\"form-control\" value=\"",PrintCodeSize,"\" id=\"codeSize\" >  
        </div>
      </div>
      </form>"],

    %CChange = "<form  method=\"post\">Set Code size:<input type=\"number\" name=\"new_code_size\"> <input type=\"submit\" value=\"Submit\"> </form>",

    Content = layout:content([List,Reg]),
    Req = cowboy_req:reply(200, #{
      <<"content-type">> => <<"text/html">>
     }, [layout:header("RP Settings"),Content], Req0),
    {ok, Req, Opts}.


get_scopes_from_consent(RPName,Consent) ->
    {ok,{_,Scopes}} = idp_mng:get_consent(RPName,Consent),
    Scopes.

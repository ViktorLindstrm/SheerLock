%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_interface_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/admin", admin_handler, []},
			{"/admin/:rp", rp_admin_handler, []},
			{"/authorize", authorize_handler, []},
			{"/token", token_handler, []},
			{"/register", register_handler, []},
			{"/introspect", introspect_handler, []},
			{"/userinfo", userinfo_handler, []},
			{"/revoke", revoke_handler, []},
            {"/[...]", cowboy_static, {priv_file, rest_interface, "login.css"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8180}], #{
		env => #{dispatch => Dispatch}
	}),
	rest_interface_sup:start_link().

stop(_State) ->
	ok.

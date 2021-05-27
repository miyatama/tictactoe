%%%-------------------------------------------------------------------
%% @doc tictactoe public API
%% @end
%%%-------------------------------------------------------------------

-module(tictactoe_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {
        "/", 
        cowboy_static, 
        {
          priv_file, 
          tictactoe, 
          "index.html"
        }
      },
      {
        "/img/[...]",
        cowboy_static,
        {priv_dir,tictactoe,"img",[{mimetypes, cow_mimetypes, all}]}
      },
      {
        "/css/[...]",
        cowboy_static,
        {priv_dir,tictactoe,"css",[{mimetypes, cow_mimetypes, all}]}
      },
      {
        "/js/[...]",
        cowboy_static,
        {priv_dir,tictactoe,"js",[{mimetypes, cow_mimetypes, all}]}
      },
      {
        "/cpu_think",
        cpu_think_h,
        []
      }
    ]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 80}],
      #{
        env => #{dispatch => Dispatch}
      }),
    tictactoe_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions

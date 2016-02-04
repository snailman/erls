%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 一月 2016 20:22
%%%-------------------------------------------------------------------
-module(erlS_sup).
-author("Administrator").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("common.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->

  logger:start(),
  log4erl:info("start root supervisor..."),

  log4erl:info("start sasl..."),
  application:start(sasl),
  log4erl:info("start sasl ok"),

  log4erl:info("start os_mon..."),
  application:start(os_mon),
  log4erl:info("start os_mon ok"),

  log4erl:info("start recon..."),
  application:start(recon),
  log4erl:info("start recon ok"),

  log4erl:info("check all process..."),
  logger:debug("~p", [[recon:info(PID) || PID <- erlang:processes()]]),
  log4erl:info("check all process ok"),

  ErlSvrCs = {
    erlS_svr_sup,
     {erlS_svr_sup, start_link, [{10,15001}]},
     permanent,
     infinity,
     supervisor,
     [erlS_svr_sup]
   },

  VmMemMonitor = {
    vm_memory_monitor,
    {vm_memory_monitor, start_link,[1]},
    permanent,
    infinity,
    worker,
    [erlS_svr_sup]
  },

  TimeServer = {
    common_time_server,
    {common_time_server, start_link,[1]},
    permanent,
    infinity,
    worker,
    [common_time_server]
  },

  {ok,
    {
      {one_for_one, 1, 10},
     [ErlSvrCs,VmMemMonitor,TimeServer]
    }
  }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
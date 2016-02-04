%%% -------------------------------------------------------------------
%%% Description : 每秒生成时间保存在ets，供所有进程获取时间
%%%
%%% -------------------------------------------------------------------
-module(common_time_server).

-behaviour(gen_server).
-include("common.hrl").
-include("common_records.hrl").


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
         start/1,
         start_link/0
        ]).

start(Sup) ->
    {ok, _} = supervisor:start_child(Sup, {?MODULE,
                                           {?MODULE, start_link, []},
                                           transient, brutal_kill, worker, 
                                           [?MODULE]}).

%% 启动子进程
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%======================================================
init([]) ->
    case erlang:whereis(?MODULE) of
        undefined ->
            erlang:register(?MODULE, self()),
            ets:new(ets_time_cache, [named_table, set, public, {keypos, #ets_time_cache.type}]),
            insert_common_time_now(refresh_common_time_now()),
            erlang:send_after(100, self(), refresh_common_time_now),
            logger:info("common_time_server start ok"),
            {ok, none};
        %%该进程已经启动了，这个进程在分布式的环境中只能有一个
        _ ->
            {stop, alread_start}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(exit, State) ->
    {stop, bad, State};

handle_info({func,Fun,Args}, State) ->
    logger:error("func Fun:~w, Args:~w", [Fun,Args]),
    Rt = apply(Fun,Args),
    logger:error("func Rt:~w", [Rt]),
    {noreply, State};



handle_info(Info, State) ->
    %% 转到自定义的do_handle_info
    do_handle_info(Info),
    {noreply, State}.

do_handle_info(refresh_common_time_now) ->
    erlang:send_after(100, self(), refresh_common_time_now),
    update_common_time_now(refresh_common_time_now());

do_handle_info(Info) ->
   logger:error("can not process this Info:~w", [Info]).

terminate(Reason, State) ->
    logger:info("terminate : ~w , reason: ~w", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    logger:info("code_change State = ~w",[State]),
    {ok, State}.

refresh_common_time_now() ->
    [Offset] = common_time:time_offset( ),
    {A, B, C} = erlang:now(),
    Now = A * 1000000 + B + Offset,
    Now2 = A * 1000000000 + (B + Offset) * 1000 + trunc(C/1000),
    {Now, Now2}.

insert_common_time_now({Now, Now2}) ->
    ets:insert(ets_time_cache, #ets_time_cache{type=?COMMON_TIME_NOW_SEC, time=Now}),
    ets:insert(ets_time_cache, #ets_time_cache{type=?COMMON_TIME_NOW_MS_SEC, time=Now2}).

update_common_time_now({Now, Now2}) ->
    ets:update_element(ets_time_cache, ?COMMON_TIME_NOW_SEC, {#ets_time_cache.time, Now}),
    ets:update_element(ets_time_cache, ?COMMON_TIME_NOW_MS_SEC, {#ets_time_cache.time, Now2}).

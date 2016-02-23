-module(common_time).

-include("common.hrl").
-include("common_records.hrl").

%% API
-export([
  time_offset/0 ,
  midnight/0 ,
  weekday/0 ,
  weekday/1 ,
  diff_next_weekday/3 ,
  diff_next_daytime/2 ,
  diff_next_time/2 ,
  week_of_year/0 ,
  week_of_year/1 ,
  time_to_date/1 ,
  diff_date/2 ,
  add_days/2 ,
  date_to_time/1 ,
  date_to_seconds/0 ,
  date_to_seconds/1 ,
  diff_next_weekday/1 ,
  diff_seconds_between_date_time/2 ,
  diff_days_between_dates/2 ,
  now/0 ,
  now2/0 ,
  time/0 ,
  date/0 ,
  next_date/0 ,
  next_date/1 ,
  pre_date/0 ,
  now_microseconds/0 ,
  now_nanosecond/0 ,
  minute_second_format/0 ,
  hour_minute_second_format/0 ,
  datetime_to_seconds/1 ,
  seconds_to_date/1 ,
  seconds_to_datetime/1 ,
  seconds_to_datetime_string/1 ,
  seconds_to_datetime_string2/0 ,
  seconds_to_datetime_string2/1 ,
  next_day/1 ,
  get_wait_time/1 ,
  get_next_second/3 ,
  localtime/0 ,
  time_format/1 ,
  date_format/0 ,
  date_format/1 ,
  is_morning/0
]).

-author('qing.liang.cn@gmail.com').

-define(OPEN , true).

-define(GREGORIAN_TIME_1970 , calendar:datetime_to_gregorian_seconds({{1970 , 1 , 1} , {8 , 0 , 0}})).

time_offset() ->
  [0].

%%@doc 增加日期
add_days(TheDate , Diff) when is_integer(Diff) andalso is_tuple(TheDate) ->
  case TheDate of
    {Date , Time} ->
      GregDate2 = calendar:date_to_gregorian_days(Date) + Diff ,
      {calendar:gregorian_days_to_date(GregDate2) , Time};
    _ ->
      GregDate2 = calendar:date_to_gregorian_days(TheDate) + Diff ,
      calendar:gregorian_days_to_date(GregDate2)
  end.


%%@doc 计算日期是当年中的第几周,简单算法
week_of_year() ->
  {_ , Week} = calendar:iso_week_number(common_time:date()) ,
  Week.
week_of_year({_Year , _Month , _Day} = Date) ->
  {_ , Week} = calendar:iso_week_number(Date) ,
  Week.

time_to_date(Time) when erlang:is_integer(Time) ->
  Date = {Time div 1000000 , Time rem 1000000 , 0} ,
  {Date2 , _} = calendar:now_to_local_time(Date) ,
  Date2.

diff_seconds_between_date_time(DateTime1 , DateTime2) ->
  calendar:datetime_to_gregorian_seconds(DateTime2) - calendar:datetime_to_gregorian_seconds(DateTime1).

diff_days_between_dates(Date1 , Date2) ->
  calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date1).

date_to_seconds() ->
  date_to_seconds(calendar:local_time()).
date_to_seconds({_Y , _M , _D} = Date) ->
  date_to_seconds({Date , {0 , 0 , 0}});
date_to_seconds({{_Y , _M , _D} = Date}) ->
  date_to_seconds({Date , {0 , 0 , 0}});
date_to_seconds(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - ?GREGORIAN_TIME_1970.

date_to_time({Date , Time}) ->
  calendar:datetime_to_gregorian_seconds({Date , Time}) - calendar:datetime_to_gregorian_seconds({{1970 , 1 , 1} , {8 , 0 , 0}});
date_to_time({Y , M , D}) ->
  calendar:datetime_to_gregorian_seconds({{Y , M , D} , {0 , 0 , 0}}) - calendar:datetime_to_gregorian_seconds({{1970 , 1 , 1} , {8 , 0 , 0}}).


diff_date(Date1 , Date2) ->
  erlang:abs(calendar:date_to_gregorian_days(Date1) - calendar:date_to_gregorian_days(Date2)).


%%今天凌晨0点的time
midnight() ->
  calendar:time_to_seconds(common_time:time()).

%%今天星期几
weekday() ->
  {Date , _} = seconds_to_datetime(common_time:now()) ,
  calendar:day_of_the_week(Date).

%%指定日期是星期几
weekday({_Y , _M , _D} = Date) ->
  calendar:day_of_the_week(Date).

%%距离到下个星期几的几时几分还有多少秒
diff_next_weekday(WeekDay , Hour , Min) ->
  Today = ?MODULE:weekday() ,
  Rtn = case Today > WeekDay of
          true ->
            (7 - Today + WeekDay) * 24 * 3600 + Hour * 3600 + Min * 60 - (calendar:time_to_seconds(common_time:time()));
          false ->
            (WeekDay - Today) * 24 * 3600 + Hour * 3600 + Min * 60 - (calendar:time_to_seconds(common_time:time()))
        end ,
  case Rtn =< 0 of
    true ->
      7 * 24 * 3600 + Rtn;
    false ->
      Rtn
  end.

%%距离到下个星期几有几天
diff_next_weekday(WeekDay) ->
  Today = ?MODULE:weekday() ,
  case Today >= WeekDay of
    true ->
      7 - Today + WeekDay;
    false ->
      WeekDay - Today
  end.

%%距离到明天的几时几分还有多少秒
diff_next_daytime(Hour , Min) ->
  86400 + Hour * 3600 + Min * 60 - (calendar:time_to_seconds(common_time:time())).

diff_next_time(Hour , Min) ->
  %% 到下一个这个时间点有多久
  {H , M , S} = common_time:time() ,
  case {Hour , Min , 0} >= {H , M , S} of
    true -> %% 指定时间 大于等于当前时间，说明下一个这个时间点还在当天
      Hour * 3600 + Min * 60 - (calendar:time_to_seconds({H , M , S}));
    _ ->
      86400 + Hour * 3600 + Min * 60 - (calendar:time_to_seconds({H , M , S}))
  end.

%% seconds
now() ->
  case ?OPEN of
    true ->
      try
        ets:lookup_element(ets_time_cache , ?COMMON_TIME_NOW_SEC , #ets_time_cache.time)
      catch
        _:_ERROR ->
          now1()
      end;
    _ ->
      now1()
  end.

now1() ->
  [Offset] = time_offset() ,%%common_config_dyn:find( time_offset, second ),
  {A , B , _} = erlang:now() ,
  A * 1000000 + B + Offset.

%% milliseconds
now2() ->
  case ?OPEN of
    true ->
      try
        ets:lookup_element(ets_time_cache , ?COMMON_TIME_NOW_MS_SEC , #ets_time_cache.time)
      catch
        _:ERROR ->
          logger:error(node() , ?MODULE , ?LINE , "common_time:now can not get time from ext which is made by common_time_server2222222222. ERROR=~w" , [ERROR]) ,
          now3()
      end;
    _ ->
      now3()
  end.
now3() ->
  [Offset] = time_offset() ,%%common_config_dyn:find( time_offset, second ),
  {A , B , C} = erlang:now() ,
  A * 1000000000 + (B + Offset) * 1000 + trunc(C / 1000).

%% microseconds
now_microseconds() ->
  {A , B , C} = erlang:now() ,
  A * 1000000000 + B * 1000 + C.

now_nanosecond() ->
  {A , B , C} = erlang:now() ,
  A * 1000000000000 + B * 1000000 + C.

time() ->
  {_Date , Time} = seconds_to_datetime(common_time:now()) ,
  Time.

date() ->
  {Date , _Time} = seconds_to_datetime(common_time:now()) ,
  Date.

pre_date() ->
  {Date , _Time} = seconds_to_datetime(common_time:now() - 86400) ,
  Date.

next_date() ->
  {Date , _Time} = seconds_to_datetime(common_time:now() + 86400) ,
  Date.

next_date(Days) ->
  {Date , _Time} = seconds_to_datetime(common_time:now() + (86400 * Days)) ,
  Date.

datetime_to_seconds({_Date , _Time} = Datetime) ->
  calendar:datetime_to_gregorian_seconds(Datetime)
  - ?GREGORIAN_TIME_1970.

seconds_to_date(MTime) ->
  {Date , _} = calendar:gregorian_seconds_to_datetime(
    ?GREGORIAN_TIME_1970 + MTime) ,
  Date.

seconds_to_datetime(MTime) ->
  calendar:gregorian_seconds_to_datetime(
    ?GREGORIAN_TIME_1970 + MTime).

seconds_to_datetime_string(MTime) ->
  {{Y , M , D} , {HH , MM , SS}} = seconds_to_datetime(MTime) ,
  io_lib:format("~w-~w-~w ~w:~w:~w" , [Y , M , D , HH , MM , SS]).

seconds_to_datetime_string2() ->
  seconds_to_datetime_string2({common_time:date() , common_time:time()}).
seconds_to_datetime_string2(MTime) when is_integer(MTime) ->
  seconds_to_datetime_string2(seconds_to_datetime(MTime));
seconds_to_datetime_string2({{Y , M , D} , {HH , MM , SS}}) ->
  lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B" , [Y , M , D , HH , MM , SS])).

%% 得到传入日期的下一天,传入的格式为{Year,Month,Day}
next_day(Datetime) ->
  {Year , Month , Day} = Datetime ,
  case lists:member(Month , [1 , 3 , 5 , 7 , 8 , 10]) of
    true ->
      case Day =:= 31 of
        true -> {Year , Month + 1 , 1};
        _ -> {Year , Month , Day + 1}
      end;
    _ ->
      case lists:member(Month , [4 , 6 , 9 , 11]) of
        true ->
          case Day =:= 30 of
            true -> {Year , Month + 1 , 1};
            _ -> {Year , Month , Day + 1}
          end;
        _ ->
          if
            Month =:= 12 ->
              case Day =:= 31 of
                true -> {Year + 1 , 1 , 1};
                _ -> {Year , Month , Day + 1}
              end;
            Month =:= 2 ->
              case calendar:is_leap_year(Year) of
                true ->
                  case Day =:= 29 of
                    true -> {Year , Month + 1 , 1};
                    _ -> {Year , Month , Day + 1}
                  end;
                _ ->
                  case Day =:= 28 of
                    true -> {Year , Month + 1 , 1};
                    _ -> {Year , Month , Day + 1}
                  end
              end
          end
      end
  end.

%% 得到距离明天特定时刻的秒数，参数格式为{hour,minute,second}
get_wait_time(Time) ->
  Now = common_time:now() ,
  {Day , _} = seconds_to_datetime(Now) ,
  NextRefreshTime = datetime_to_seconds({next_day(Day) , Time}) ,
  NextRefreshTime - Now.

%% 获得距离每天指定时间点的秒数
get_next_second(Nh , Nm , Ns) ->
  {_ , {H , M , S}} = erlang:localtime() ,
  NowDateSecond = H * 60 * 60 + M * 60 + S ,
  ActionSecond = Nh * 60 * 60 + Nm * 60 + Ns ,
  NextSecond = if NowDateSecond >= ActionSecond ->
    24 * 60 * 60 - (NowDateSecond - ActionSecond);
                 true ->
                   ActionSecond - NowDateSecond
               end ,
  NextSecond.

%% 得到系统时间
localtime() ->
  datetime_to_seconds(erlang:localtime()).

%% time format,
one_to_two(One) -> io_lib:format("~2..0B" , [One]).

%%@doc 获取时间格式
time_format(Now) ->
  {{Y , M , D} , {H , MM , S}} = calendar:now_to_local_time(Now) ,
  lists:concat([Y , "-" , one_to_two(M) , "-" , one_to_two(D) , " " ,
                one_to_two(H) , ":" , one_to_two(MM) , ":" , one_to_two(S)]).

%%@doc 获取日期格式
date_format() ->
  {Y , M , D} = common_time:date() ,
  lists:concat([Y , "-" , one_to_two(M) , "-" , one_to_two(D)]).
%%@doc 获取日期格式
date_format({Y , M , D}) ->
  lists:concat([Y , "-" , one_to_two(M) , "-" , one_to_two(D)]).

%%@doc 获取时间的分秒格式
minute_second_format() ->
  Now = erlang:now() ,
  {{_Y , _M , _D} , {H , MM , _S}} = calendar:now_to_local_time(Now) ,
  lists:concat([one_to_two(H) , "-" , one_to_two(MM)]).

%%@doc 获取时间的时分秒格式
hour_minute_second_format() ->
  Now = erlang:now() ,
  {{_Y , _M , _D} , {H , MM , S}} = calendar:now_to_local_time(Now) ,
  lists:concat([one_to_two(H) , ":" , one_to_two(MM) , ":" , one_to_two(S)]).

%%@doc 获取当前时间是上午还是下午，以中午12点区分
is_morning() ->
  {_ , {H , M , S}} = erlang:localtime() ,
  NowDateSecond = H * 60 * 60 + M * 60 + S ,
  ActionSecond = 12 * 60 * 60 ,
  case NowDateSecond =< ActionSecond of
    true ->
      true;
    _ ->
      false
  end.

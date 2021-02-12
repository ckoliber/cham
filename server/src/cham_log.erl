-module(cham_log).
-author("koliber").
-export([log/1]).

-spec log(list()) -> tuple().
log(LOG) ->
  try
    io:fwrite(LOG),
    {ok}
  catch
    _:Error -> {no,Error}
  end.
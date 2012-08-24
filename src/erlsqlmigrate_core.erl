-module(erlsqlmigrate_core).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([create/3,
         up/3,
         down/3
        ]).

-include("migration.hrl").
%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------
-define(UPDIR(MigDir,Driver),filename:join([MigDir,Driver,"up"])).
-define(DOWNDIR(MigDir,Driver),filename:join([MigDir,Driver,"down"])).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
create([{Driver,_ConnArgs}]=Config,MigDir,Name) ->
    filelib:ensure_dir(?UPDIR(MigDir,Driver)++"/"),
    filelib:ensure_dir(?DOWNDIR(MigDir,Driver)++"/"),
    Migration= get_migration(Driver,MigDir,Name),
    file:write_file(Migration#migration.up_path, <<"">>),
    file:write_file(Migration#migration.down_path, <<"">>),
    run_driver(Config,create,[MigDir,Name,Migration]).

up([{Driver,_ConnArgs}]=Config, MigDir, Name) ->
    Regex = case Name of
                [] -> "*";
                Name -> "*"++Name++"*"
            end,
    %% I think its sorted but anyway
    Files = lists:sort(filelib:wildcard(?UPDIR(MigDir,Driver)++"/"++Regex)),
    Migrations = get_migrations(Driver, MigDir, Files),
    run_driver(Config, up, Migrations).

down([{Driver,_ConnArgs}]=Config, MigDir, Name) ->
    Regex = case Name of
                [] -> throw(no_down_specified);
                Name -> "*"++Name++"*"
            end,
    %% I think its sorted but anyway
    Files = lists:sort(filelib:wildcard(?UPDIR(MigDir,Driver)++"/"++Regex)),
    [Migration] = get_migrations(Driver, MigDir, Files),
    io:format("~nMig:~p~n",[Migration]),
    run_driver(Config, down, Migration).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

run_driver([{pgsql,ConnArgs}],Cmd,Args) ->
    erlsqlmigrate_driver_pg:Cmd(ConnArgs,Args);
run_driver([{_,_ConnArgs}],_Cmd,_Args) ->
    throw(unknown_database).

get_migration(Driver, MigDir, {Name, Timestamp, Up, Down}) ->
    Title = integer_to_list(Timestamp)++"-"++Name,
    #migration{date=Timestamp,
               name=Name,
               title=Title,
               up_path=?UPDIR(MigDir,Driver)++"/"++Title++".sql",
               down_path=?DOWNDIR(MigDir,Driver)++"/"++Title++".sql",
               up = Up,
               down = Down
              };
get_migration(Driver, MigDir, Name) ->
    Timestamp = erlsqlmigrate_utils:get_timestamp(),
    get_migration(Driver, MigDir, {Name, Timestamp, [], []}).

get_migrations(Driver, MigDir, Files) ->
    lists:map(
      fun(F) ->
          case re:run(F,"\/([0-9]+)\-(.*)\.sql\$",[{capture,all_but_first,list}]) of
              {match,[Timestamp,Name]} ->
                  Up = readlines(F),
                  Fd = re:replace(F, "/up/", "/down/", [global, {return, list}]),
                  Down = readlines(Fd),
                  get_migration(Driver, MigDir,
                                {Name,list_to_integer(Timestamp),Up,Down});
              nomatch -> throw(file_not_a_migration_file)
          end
      end,
      Files ).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.

%% @author Dipesh Patel <dipthegeezer.opensource@gmail.com>
%% @copyright 2012 Dipesh Patel.

%% @doc The core of erlsqlmigrate, find migrations to apply.

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
%% @spec create([{DB,ConnArgs}], MigDir, Name) -> ok
%%       DB = atom()
%%       ConnArgs = term()
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Creates the migration files ready to be filled in with SQL.
%% Also calls the Database driver in case there is any setup needed there.
create([{Driver,_ConnArgs}]=Config,MigDir,Name) ->
    filelib:ensure_dir(?UPDIR(MigDir,Driver)++"/"),
    filelib:ensure_dir(?DOWNDIR(MigDir,Driver)++"/"),
    Migration= get_migration(Driver,MigDir,Name),
    file:write_file(Migration#migration.up_path, <<"">>),
    file:write_file(Migration#migration.down_path, <<"">>),
    run_driver(Config,create,[MigDir,Name,Migration]).

%% @spec up([{DB,ConnArgs}], MigDir, Name) -> ok
%%       DB = atom()
%%       ConnArgs = term()
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Run the up migration. Fetch migration files and pass to driver.
up([{Driver,_ConnArgs}]=Config, MigDir, Name) ->
    Regex = case Name of
                [] -> "*";
                Name -> "*"++Name++"*"
            end,
    %% I think its sorted but anyway
    Files = lists:sort(filelib:wildcard(?UPDIR(MigDir,Driver)++"/"++Regex)),
    Migrations = get_migrations(Driver, MigDir, Files),
    run_driver(Config, up, Migrations).

%% @spec down([{DB,ConnArgs}], MigDir, Name) -> ok
%%       DB = atom()
%%       ConnArgs = term()
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Run the down migration. Fetch migration files and pass to driver.
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

%% @spec run_driver([{DB,ConnArgs}],Cmd,Args) -> ok
%%       DB = atom()
%%       ConnArgs = term()
%%       Cmd = atom()
%%       Args = any()
%%
%% @throws unknown_database
%% @doc Runs command on the driver for the specified database
run_driver([{pgsql,ConnArgs}],Cmd,Args) ->
    erlsqlmigrate_driver_pg:Cmd(ConnArgs,Args);
run_driver([{_,_ConnArgs}],_Cmd,_Args) ->
    throw(unknown_database).

%% @spec get_migration(Driver, MigDir, {Name, Timestamp, Up, Down}) -> Record :: #migration{}
%%       Driver = atom()
%%       MigDir = filelib:dirname()
%%       Name = string()
%%       TimeStamp = integer()
%%       Up = string()
%%       Down = string()
%%
%% @doc Creates a migration records from a set of parameter
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


%% @spec get_migrations(Driver, MigDir, Files) -> [Record :: #migration{}] | {error, errorinfo()}
%%       Driver = atom()
%%       MigDir = filelib:dirname()
%%       Files = [file:filename()]
%%
%% @doc Creates a list of migration records from files
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

%% @spec readlines(FileName) -> string() | {error, errorinfo()}
%%       FileName = file:filename()
%%
%% @doc Reads the entire file given by FileName into a string
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

%% @spec get_all_lines(Device, Accum) -> string() | {error, errorinfo()}
%%       Device = file:io_device()
%%       Accum = string()
%% @doc Gets lines recursively from the device and returns it as string()
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.

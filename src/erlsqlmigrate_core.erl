-module(erlsqlmigrate_core).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([create/3,
         up/0,
         down/0]).

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
    {ok, Migration }= get_migration(Driver,MigDir,Name),
    file:write_file(Migration#migration.up_path, <<"">>),
    file:write_file(Migration#migration.down_path, <<"">>),
    run_driver(Config,create,[MigDir,Name,Migration]),
    ok.

up() ->
    ok.

down() ->
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

run_driver([{pgsql,ConnArgs}],Cmd,Args) ->
    erlsqlmigrate_driver_pg:Cmd(ConnArgs,Args).


get_migration(Driver,MigDir,Name) ->
    Timestamp = erlsqlmigrate_utils:get_timestamp(),
    Title = integer_to_list(Timestamp)++"-"++Name,
    {ok, #migration{date=Timestamp,
                    name=Name,
                    title=Title,
                    up_path=?UPDIR(MigDir,Driver)++"/"++Title++".sql",
                    down_path=?DOWNDIR(MigDir,Driver)++"/"++Title++".sql"
                   }}.

%% @author Dipesh Patel <dipthegeezer.opensource@gmail.com>
%% @copyright 2012 Dipesh Patel.

%% @doc The entry point into migrating a database.

-module(erlsqlmigrate).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([main/1]).
-export([create/3,up/3,down/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

main([]) ->
    usage();

main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            case parse_command_args(NonOptArgs) of
                {Cmd,Name} ->
                    case run(Options, Cmd, Name) of
                        ok ->
                            ok;
                        Error ->
                            io:format("Uncaught error: ~p\n", [Error])
                    end;
                error -> usage(OptSpecList)
            end;
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            usage(OptSpecList)
    end.

%% @type config() = {atom(),list()}.
%% The config is composed of the database type and any parameters needed
%% to start the database connection.

%% @spec create(Config, MigDir, Name) -> ok
%%       Config = config()
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Create a set of migration files and also run any Database specific
%% tasks.
create(Config, MigDir, Name) ->
    io:format("~nConfig:~n  ~p~n~nName:~n  ~p~n ~p~n", [Config,Name,MigDir]),
    erlsqlmigrate_core:create(Config, MigDir, Name).

%% @spec up(Config, MigDir, Name) -> ok
%%       Config = config()
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Run the 'up' migration. The name can be an empty string in which case
%% all un-applied migrations will run
up(Config, MigDir, Name) ->
    io:format("~nConfig:~n  ~p~n~nName:~n  ~p~n ~p~n", [Config,Name,MigDir]),
    erlsqlmigrate_core:up(Config,MigDir,Name).

%% @spec down(Config, MigDir, Name) -> ok
%%       Config = config
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Run the 'down' migration. The name can be an empty string in which case
%% all applied migrations will be un-applied.
down(Config, MigDir, Name) ->
    io:format("~nConfig:~n  ~p~n~nName:~n  ~p~n ~p~n", [Config,Name,MigDir]),
    erlsqlmigrate_core:down(Config, MigDir, Name).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @spec usage() -> ok
%% @equiv usage(OptSpecList)
usage() ->
    usage(option_spec_list()).

%% @spec usage(OptSpecList) -> ok
%%       OptSpecList = [getopt:option_spec()]
%%
%%
%% @doc Print out the usage commands to standard error.
usage(OptSpecList) ->
    getopt:usage(OptSpecList, escript:script_name(), "[up, down, create] [name]",
                 [{"command",   "Migration command"},
                 {"name", "Optional migration name"}]).

%% @spec option_spec_list() -> [getopt:option_spec()]
%%
%% @doc Return the specification of the command line arguments.
option_spec_list() ->
    {ok, Cwd } = file:get_cwd(),
    MigrationsDefault = filename:join([Cwd,"migrations"]),
    ConfigDefault = filename:join([Cwd,"config"]),

    [
     {help,            $h, "help",          undefined,                    "Show the program options"},
     {verbose,         $v, "verbose",       {boolean, false},             "Be verbose about what gets done"},
     {version,         $V, "Version",       {boolean, false},             "output the version number"},
     {environment,     $e, "environment",   {string,"development"},       "Environment you are running migration on. Defaults to 'development'"},
     {migration_dir,   $m, "migration-dir", {string, MigrationsDefault},  "The directory containing your SQL migration files [./migrations]"},
     {config_dir,      $c, "config-dir",    {string, ConfigDefault},      "Location of your config files [./config]"}
    ].

run(Options, Cmd, Name) ->
    io:format("Options:~n  ~p~n~nNon-option arguments:~n  ~p~p~n", [Options, Cmd, Name]),
    ConfigDir = proplists:get_value(config_dir, Options),
    Environment = proplists:get_value(environment, Options),
    MigDir = proplists:get_value(migration_dir, Options),
    {ok, Config} = file:consult(filename:join([ConfigDir,Environment++".config"])),
    Mod = ?MODULE,
    Mod:Cmd(Config, MigDir, Name).

parse_command_args(["create", Name]) ->
    {create,Name};
parse_command_args(["create"]) ->
    io:format("~nMust specify a name with `create` command~n~n"),
    error;
parse_command_args(["down", Name]) ->
    {down,Name};
parse_command_args(["down"]) ->
    {down,[]};
parse_command_args(["up"]) ->
    {up,[]};
parse_command_args(Args) ->
    io:format("~nUnknown Commands:~p~n~n",[Args]),
    error.

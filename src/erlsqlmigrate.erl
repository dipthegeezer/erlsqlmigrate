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

%% @spec main(Args) -> ok
%%       Args = string() | [string()]
%%
%% @throws unknown_database
%% @doc Entry point when running as a script. Parse the args passed in
%% from the command line and works out what to do.
main(Args) ->
    ok = load_app(),
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            try(run(Options, NonOptArgs)) of
                ok ->
                    ok;
                Error ->
                    io:format("Uncaught error: ~p\n", [Error])
            catch
                setup_error -> io:format("~nERROR: Setup not performed please run 'create' command.~n~n", [])
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
    erlsqlmigrate_core:create(Config, MigDir, Name).

%% @spec up(Config, MigDir, Name) -> ok
%%       Config = config()
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Run the 'up' migration. The name can be an empty string in which case
%% all un-applied migrations will run
up(Config, MigDir, Name) ->
    erlsqlmigrate_core:up(Config,MigDir,Name).

%% @spec down(Config, MigDir, Name) -> ok
%%       Config = config
%%       MigDir = filelib:dirname()
%%       Name = string()
%% @throws unknown_database
%% @doc Run the 'down' migration. The name can be an empty string in which case
%% all applied migrations will be un-applied.
down(Config, MigDir, Name) ->
    erlsqlmigrate_core:down(Config, MigDir, Name).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @spec version() -> ok
%% @doc print version information.
%%
version() ->
    {ok, Vsn} = application:get_key(erlsqlmigrate, vsn),
    io:format("erlsqlmigrate ~p \n", [Vsn]).

%% @spec load_app() -> ok
%% @doc Pre-load the app so that we get default configuration
load_app() ->
    ok = application:load(erlsqlmigrate).

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

%% @spec run(Options, NonOptArgs) -> ok
%%       Options = [proplists:property()]
%%       NonOptArgs = [string()]
%%
%% @doc Run the migration give the commands and arguments provided.
run(Options, NonOptArgs) ->
    case proplists:get_value(version, Options) of
        true -> version(), ok;
        false -> case parse_command_args(NonOptArgs) of
                     {Cmd,Name} ->
                         ConfigDir = proplists:get_value(config_dir, Options),
                         Environment = proplists:get_value(environment, Options),
                         MigDir = proplists:get_value(migration_dir, Options),
                         {ok, Config} = file:consult(filename:join([ConfigDir,Environment++".config"])),
                         Mod = ?MODULE,
                         Mod:Cmd(Config, MigDir, Name);
                     error -> usage()
                 end
    end.

%% @spec parse_command_args(List) -> {atom(),string()} | error
%%       List = [string()]
%%
%% @doc Parse the list of commands into a form that can be used
parse_command_args(["create", Name]) ->
    {create,Name};
parse_command_args(["create"]) ->
    {create,[]};
parse_command_args(["down", Name]) ->
    {down,Name};
parse_command_args(["down"]) ->
    {down,[]};
parse_command_args(["up"]) ->
    {up,[]};
parse_command_args(Args) ->
    io:format("~nUnknown Commands:~p~n~n",[Args]),
    error.

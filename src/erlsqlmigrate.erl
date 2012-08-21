-module(erlsqlmigrate).

-export([main/1]).

%% ====================================================================
%% Public API
%% ====================================================================
main([]) ->
    usage();

main(Args) ->
    case catch(run(Args)) of
        ok ->
            ok;
        Error ->
            io:format("Uncaught error: ~p\n", [Error])
    end.

usage() ->
    usage(option_spec_list()).

usage(OptSpecList) ->
    getopt:usage(OptSpecList, escript:script_name(), "[up,down,create] [name]",
                 [{"command",   "Migration command"},
                 {"name", "Optional migration name"}]).

option_spec_list() ->
    [
     {help,            $h, "help",          undefined,             "Show the program options"},
     {verbose,         $v, "verbose",       {boolean, false},      "Be verbose about what gets done"},
     {version,         $V, "Version",       {boolean, false},      "output the version number"},
     {migrations_dir,  $m, "migration-dir", string,                "The directory containing your SQL migration files [./migrations]"},
     {config_dir,      $c, "config-dir",    string,                "Location of your config files [./config]"}
    ].

run(_Args) ->
    ok.

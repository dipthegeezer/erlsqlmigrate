%% @author Aaron Spiegel <spiegela@gmail.com>
%% @copyright 2014 Aaron Spiegel.

%% @doc The mysql driver file, uses Eonblast/Emysql

-module(erlsqlmigrate_driver_my).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([create/2, up/2, down/2]).

-import(emysql, [execute/2, as_json/1]).

-include("migration.hrl").

%% Emysql Results
%% Opted not to import since we don't want to my this an application dependency
%% for Postgres users
-record(ok_packet, {seq_num :: number(),
                    affected_rows :: number(),
                    insert_id :: number(),
                    status :: number(),
                    warning_count :: number(),
                    msg :: string()
                         | {error, string(), unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata()}
                         | {incomplete, string(), binary()}}).

-record(result_packet, {seq_num :: number(), 
			field_list :: list(),
			rows, extra}).

-record(error_packet, {seq_num :: number(), 
		       code :: number(), 
		       status :: binary(), 
		       msg :: [byte()]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @spec create(Config, Args) -> ok
%%       Config = erlsqlmigrate:config()
%%       Args = [any()]
%%
%%
%% @doc Create the migration table in the Database if it doesn't
%% already exist
create(ConnArgs,_Args) ->
  Conn = connect(ConnArgs),
  case is_setup(Conn) of
    true -> ok;
    false ->
      #ok_packet{} = execute(Conn, "CREATE TABLE migrations(title VARCHAR(255) PRIMARY KEY,updated TIMESTAMP)"),
      ok
  end,
  ok = disconnect(Conn),
  ok.

%% @spec up(Config, Migrations) -> ok
%%       Config = erlsqlmigrate:config()
%%       Migrations = [erlsqlmigrate_core:migration()]
%%
%% @throws setup_error
%%
%% @doc Execute the migrations in the given list of migrations.
%% Executions will not be wrapped in transactions due to limitations of
%% MySQL's transaction system. The migrations table will be updated to
%% indicate it has been executed. Note if the migration has already been
%% applied it will be skipped.
%% Ref: http://dev.mysql.com/doc/refman/5.1/en/implicit-commit.html
up(ConnArgs, Migrations) ->
  Conn = connect(ConnArgs),
  lists:foreach(
    fun(Mig) ->
        case applied(Conn, Mig) of
          true  -> ok;
          false -> Fun = fun() -> update(Conn,Mig) end,
            execute_with_success(Conn, Mig#migration.up, Fun)
        end
    end, Migrations),
  ok = disconnect(Conn),
  ok.

%% @spec down(Config, Migrations) -> ok
%%       Config = erlsqlmigrate:config()
%%       Migrations = [erlsqlmigrate_core:migration()]
%%
%% @throws setup_error
%%
%% @doc Execute the down migrations in the given list of migrations.
%% Executions will not be wrapped in transactions due to limitations of
%% MySQL's transaction system. The migrations table will have the migration
%% entry removed to indicate it has been executed. Note if the up migration
%% has not been applied it will be skipped.
%% Ref: http://dev.mysql.com/doc/refman/5.1/en/implicit-commit.html
down(ConnArgs, Migrations) ->
  Conn = connect(ConnArgs),
  case is_setup(Conn) of
    true -> ok;
    false -> throw(setup_error)
  end,
  lists:foreach(
    fun(Mig) ->
        case applied(Conn, Mig) of
          false -> io:format("Skiping ~p it has not been applied~n.",
              [Mig#migration.title]),
            ok;
          true -> Fun = fun() -> delete(Conn,Mig) end,
            execute_with_success(Conn, Mig#migration.down, Fun)
        end
    end, Migrations),
  ok = disconnect(Conn),
  ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @spec disconnect(Conn) -> ok
%%
%% @doc Close existing connection to the mysql database
disconnect(Conn) ->
  emysql:remove_pool(Conn).

%% @spec connect(ConnArgs) -> pid()
%%       ConnArgs = list()
%%
%% @doc Connect to the mysql database using mysql-driver
connect([Hostname, Port, Database, Username, Password]) ->
  application:ensure_started(emysql),
  case emysql:add_pool(migration_pool, [ {host, Hostname},
                                         {user, Username},
                                         {password, Password},
                                         {database, Database},
                                         {port, Port}
                                       ]) of
    ok -> migration_pool;
    {error, Error} -> throw(Error)
  end.

%% @spec transaction(Conn, Sql, Fun) -> ok
%%       Conn = pid()
%%       Sql = string()
%%       Fun = fun()
%%
%% @doc Execute a sql statement, and also perform execute any other function
%% if the execution was successful.  Note that no rollback can occur due to
%% the likelyhood that the migration SQL will contain features with implicit
%% commits.
execute_with_success(Conn, Sql, Fun) ->
  #ok_packet{} = execute(Conn, Sql),
  Fun(),
  ok.

execute(Conn, Title, Sql, Params) when is_binary(Title) ->
  execute(Conn, binary_to_list(Title), Sql, Params);
execute(Conn, Title, Sql, Params) when is_list(Title) ->
  execute(Conn, list_to_atom(Title), Sql, Params);
execute(Conn, Title, Sql, Params) when is_atom(Title) ->
  emysql:prepare(Title, Sql),
  case emysql:execute(Conn, Title, Params) of
    #ok_packet{}       -> ok;
    #result_packet{}=R -> R;
    #error_packet{}=E  -> error(E#error_packet.msg)
  end.
%% @spec update(Conn, Migration) -> ok
%%       Conn = pid()
%%       Migration = erlsqlmigrate_core:migration()
%%
%% @doc Insert into the migrations table the given migration.
update(Conn,Migration) ->
  Title = iolist_to_binary(Migration#migration.title),
  execute(Conn, add_migration,
    "INSERT INTO migrations (title, updated) VALUES(?, now())",
    [Title]).

%% @spec delete(Conn, Migration) -> ok
%%       Conn = pid()
%%       Migration = erlsqlmigrate_core:migration()
%%
%% @doc Delete the migrations table entry for the given migration
delete(Conn,Migration) ->
  Title = iolist_to_binary(Migration#migration.title),
  execute(Conn, remove_migration,
    "DELETE FROM migrations where title = ?",
    [Title]).

%% @spec applied(Conn, Migration) -> ok
%%       Conn = pid()
%%       Migration = erlsqlmigrate_core:migration()
%%
%% @doc Check whether the given migration has been applied by
%% querying the migrations table
applied(Conn, Migration) ->
  Title = iolist_to_binary(Migration#migration.title),
  Result = execute(Conn, applied_title,
                   "SELECT * FROM migrations WHERE title = ?;", [Title]),
  case as_json(Result) of
    [_Row] -> true;
    [] -> false
  end.

%% @spec is_setup(Conn) -> true | false
%%       Conn = pid()
%%
%% @doc Simple function to check if the migrations table is set up
%% correctly.
is_setup(Conn) ->
  #result_packet{} = Result = execute(Conn, "SELECT table_name FROM INFORMATION_SCHEMA.tables WHERE table_name=\"migrations\" AND table_schema = DATABASE()"),
  case as_json(Result) of
    [_Row] -> true;
    [] -> false
  end.


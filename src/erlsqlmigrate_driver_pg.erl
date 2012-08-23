-module(erlsqlmigrate_driver_pg).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([create/2, up/2]).

-include("migration.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
create(ConnArgs,_Args) ->
    Conn = connect(ConnArgs),
    case squery(Conn, "SELECT * FROM pg_tables WHERE tablename='migrations'") of
        {ok, _Cols, [_Row]} -> ok;
        {ok, _Cols, []} ->
            {ok,[],[]} = squery(Conn, "CREATE TABLE migrations(title TEXT PRIMARY KEY,updated TIMESTAMP)"),
            ok
    end.

up(ConnArgs, Migrations) ->
    Conn = connect(ConnArgs),
    lists:foreach(
      fun(Mig) ->
          case applied(Conn, Mig) of
              true  -> ok;
              false -> Fun = fun() -> update(Conn,Mig) end,
                       transaction(Conn, Mig#migration.up, Fun)
          end
      end, Migrations),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

connect([Hostname,Port,Database,Username,Password]) ->
    case pgsql:connect( Hostname, Username, Password,
                        [{database, Database}, {port, Port}]
                      ) of
        {ok,Conn} -> Conn;
        {error, Error} -> throw(Error)
    end.

transaction(Conn, Sql, Fun) ->
    squery(Conn, "BEGIN"),
    squery(Conn, Sql),
    Fun(),
    squery(Conn, "COMMIT"),
    ok.

squery(Conn, Sql) ->
    case pgsql:squery(Conn, Sql) of
        {error, Error} -> throw(Error);
        Result -> Result
    end.

pquery(Conn, Sql, Params) ->
    case pgsql:pquery(Conn, Sql, Params) of
        {error, Error} -> throw(Error);
        Result -> Result
    end.

update(Conn,Migration) ->
    Title = iolist_to_binary(Migration#migration.title),
    pquery(Conn, "INSERT INTO migrations(title,updated) VALUES($1,now())",
           [Title]).

applied(Conn, Migration) ->
    Title = iolist_to_binary(Migration#migration.title),
    case pquery(Conn, "SELECT * FROM migrations where title=$1",[Title]) of
        {ok, _Cols, [_Row]} -> true;
        {ok, _Cols, []} -> false
    end.

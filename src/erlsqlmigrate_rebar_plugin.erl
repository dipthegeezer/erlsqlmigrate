-module(erlsqlmigrate_rebar_plugin).

-export(['create-migration'/2, 'migrate-up'/2, 'migrate-down'/2]).

'create-migration'(_Config, AppFile) ->
  {Dir, Conf} = config(AppFile),
  erlsqlmigrate:create([Conf], Dir, get_name_arg(args())).

'migrate-up'(_Config, AppFile) ->
  {Dir, Conf} = config(AppFile),
  erlsqlmigrate:up([Conf], Dir, get_name_arg(args())).

'migrate-down'(_Config, AppFile) ->
  {Dir, Conf} = config(AppFile),
  erlsqlmigrate:down([Conf], Dir, get_name_arg(args())).

config(AppFile) ->
  {ok, [{application, _, AppSrc}]} = file:consult(AppFile),
  {env, Env}= proplists:lookup(env, AppSrc),
  Db  = lists:foldl(fun(E, P) ->
                      {E, P1} = proplists:lookup(E, P), P1
                    end, Env, [migrate, environment()] ),
  {migration_dir, Dir} = proplists:lookup(migration_dir, Env),
  {Dir, Db}.

environment() ->
  case os:getenv("ENV") of
    false -> development;
    Env   -> list_to_atom(Env)
  end.

get_name_arg([<<"name">>, Name]) -> binary_to_list(Name);
get_name_arg([]) -> "".

args() ->
  case init:get_plain_arguments() of
    [_, _] -> [];
    [_, _, ArgStr] -> init:get_plain_arguments(), re:split(ArgStr, "=")
  end.

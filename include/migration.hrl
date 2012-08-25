
%% @type migration() = Record :: #migration{ date = integer(),
%%                                 name = string(),
%%                                 title = string(),
%%                                 up_path = filelib:dirname(),
%%                                 down_path = filelib:dirname(),
%%                                 up = string(),
%%                                 down = string()}.

-record(migration, { date :: integer(),
                     name :: string(),
                     title :: string(),
                     up_path :: filelib:dirname(),
                     down_path :: filelib:dirname(),
                     up :: string(),
                     down :: string()}).

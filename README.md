erlsqlmigrate
=============

Database migration framework for Erlang

## Usage

### Command Line

```
Usage: erlsqlmigrate [options] [up|down|create] migrationName

Options:
  -h, --help		Show the program options
  -v, --verbose		Be verbose about what gets done
  -V, --Version		output the version number
  -e, --environment	Environment you are running migration on. Defaults to 'development'
  -m, --migration-dir	The directory containing your SQL migration files [./migrations]
  -c, --config-dir	Location of your config files [./config]

```

### API

If you want to run the migrations from your code. There is an API available.

```
erlsqlmigrate:create(Config, MigDir, Name)

erlsqlmigrate:up(Config, MigDir, Name)

erlsqlmigrate:down(Config, MigDir, Name)

```

## Creating Migrations

    $ erlsqlmigrate create add-users

This will create a two files ./migrations/up/<datestamp>_add-users.sql and ./migrations/down/<datestamp>_add-users.sql.

All you have to do is populate these with your SQL Command and you are ready to migrate.

For example in up/add-users.sql:

CREATE TABLE users( id SERIAL PRIMARY KEY, email TEXT UNIQUE NOT NULL );

and in down/add-users.sql

DROP TABLE users

Then just run the migration

    $ erlsqlmigrate up [add-users]

If there is a problem then rollback. Note if you don't supply a name it will rollback all the migrations.

    $ erlsqlmigrate down [add-users]


## License

(The MIT License)

Copyright (c) 2011 dipthegeezer

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
Postgres-JSON
===============

## What is it?

A Common Lisp library that provides a user friendly layer over the new
[jsonb type](http://www.postgresql.org/docs/9.4/static/datatype-json.html)
of PostgreSQL 9.4, allowing trivial storage and retrieval of
JSON documents.  Thanks to the excellent JSON libraries for Common
Lisp, Postgres-JSON thus facilitates easy serialization of lisp data
structures to and from a proper database.

## Why would you use it?

1. You have some existing JSON documents you want to store persistently.

2. You want to serialize Common Lisp hash tables, lists, vectors,
et cetera to a database.

3. You like the [ACID](http://en.wikipedia.org/wiki/ACID) qualities of
PostgreSQL but rigid data schema are not suitable for your project.

In some sense Postgres-JSON is a primitive *NoSQL document database*.
It was inspired by the excellent
[cl-rethinkdb](https://github.com/orthecreedence/cl-rethinkdb)
interface to [RethinkDB](http://rethinkdb.com/) but uses a traditional
blocking I/O interface via Postmodern.

## Built on

* [PostgreSQL 9.4](http://www.postgresql.org).

* Marijn Haverbeke's wonderful [Postmodern](http://marijnhaverbeke.nl/postmodern/).

* Any Common Lisp JSON library.
[Yason](http://common-lisp.net/project/yason/) is a dependency of this
project but you can use whatever library you like:
see the comparison by Sabra On The Hill: [JSON libraries]
(https://sites.google.com/site/sabraonthehill/home/json-libraries).

## Status

As at January 2015 and according to the langauge of [Semantic
versioning](http://semver.org) Postgres-JSON is in the "initial
development phase".  Anything might change.

That said, the interface is relatively stable and the documentation
largely complete.  So do not use it in production but any and all
feedback is most welcome to postgres-json@gtod.net or by raising a
GitHub issue.

## Documentation

* [Beginner's guide to JSON with Common Lisp]
(doc/beginners.md)
* [User's Guide](doc/user-guide.md)
* [API](doc/api.md)

Most of the library code has docstrings.

## Quickstart

#### Postgres

You will need a working PostgreSQL 9.4 install.  On Debian this may be
as simple as `apt-get install postgresql-9.4`.  If this does not work, see
https://wiki.postgresql.org/wiki/Apt for help updating your apt sources.

Once installed, try `pg_lsclusters` to see what port your 9.4 install
is on, if it is not 5432 you will need to explicitly supply the port
as I have in the example below.  `pg_upgradecluster` may have been
automatically run for you, in which case your new install may already
be on port 5432.

If this is your first time using Postgres you can setup a database
user to match your unix login (in my case `gtod`) at the unix shell as
follows:

```
sudo su postgres
createuser gtod
createdb -O gtod mydb
exit
```

`psql -l` or `psql -p5433 -l` should now list your new database.

For passwordless Postmodern connections I edit the
`/etc/postgresql/9.4/main/pg_hba.conf` file (which may be elsewhere on
non Debian systems).  There is a line:

```
host    all             all             127.0.0.1/32            md5
```

Change `md5` to `trust`.  See [auth
trust](http://www.postgresql.org/docs/9.4/static/auth-methods.html#AUTH-TRUST)
for the pros and cons of such an approach.

Then `sudo service postgresql restart`.  Again, may be different on
non Debian systems.

#### Postgres-JSON

Navigate to your `~/quicklisp/local-projects` directory and do

`git clone https://github.com/gtod/postgres-json.git`.  Then at your
REPL evaluate:

```common-lisp
(ql:register-local-projects)
(ql:quickload :postgres-json)
```

Now:

```common-lisp
(defpackage :simple
  (:use :cl :postgres-json))

(in-package :simple)

;; Change to suit your Postgres DB
(setf *postmodern-connection* '("mydb" "gtod" "" "localhost" :port 5433))
(ensure-top-level-connection)

;; Create a Postgres-JSON model (and global instance) to store cats
(define-global-model cat -cat- (pgj-object-model))

;; Ensure there is a database backend for our cat model
(ensure-backend -cat-)
```

In the output below I have elided some of the return values for
brevity.  `obj` is a trivial function to turn a list of pairs into a
hash table.  `pp-json` is a trivial function to pretty print a nested
lisp object of hash tables and sequences as JSON.

```common-lisp
(insert -cat- (obj "name" "joey" "coat" "tabby"))
1

(pp-json (fetch -cat- 1))
{
    "key":1,
    "coat":"tabby",
    "name":"joey"
}

(insert -cat- (obj "name" "max" "coat" "ginger"))

(insert -cat- (obj "name" "maud" "coat" "tortoiseshell"))

(keys -cat-)
(1 2 3)

(excise -cat- 2)
2

(keys -cat-)
(1 3)

(tally -cat-)
2

(supersede -cat- 3 (obj "name" "maud" "coat" "tortoiseshell" "age" 7
                        "likes" '("sunshine" 42)))
3

(pp-json (fetch -cat- 3))
{
    "age":7,
    "key":3,
    "coat":"tortoiseshell",
    "name":"maud",
    "likes":[
        "sunshine",
        42
    ]
}
```

#### Examples

See [simple](examples/simple.lisp) for similar code to the above.
There is an extended example in [human-1](examples/human-1.lisp) and
[human-2](examples/human-2.lisp).  An example of the simple
customizations available by specializing generic functions is shown in
[customize](examples/customize.lisp).  `(ql:quickload
:postgres-json-examples)` will compile all the examples.

An example user defined query from [human-2](examples/human-2.lisp)
and documented in [User defined JSON
queries](doc/user-guide.md#user-defined-json-queries):

```common-lisp
(define-json-query uncharitable-humans$ ()
  (:select (jbuild (human "name") (gift "type" "quantity"))
   :from 'human
   :inner-join 'gift
   :on (:= (j-> human "key") (j-> gift "human-key"))
   :where (:= (j-> gift "quantity") (to-jsonb 1))))
```

 A reminder that none of this will work unless
`*postmodern-connection*` is set correctly for your Postgres database.

## Features

#### Immutability

By writing

```common-lisp
(define-global-model cat -cat- (pgj-history-object-model))
```

our `cat` model descends from a class which maintains history.  Now when
you call [`supersede`](doc/api.md#supersede) (which means *replace*
but is not a Common Lisp standard symbol) or
[`excise`](doc/api.md/#excise) (which means *delete*...) a [JSON
document](doc/user-guide.md#json-document) in a
[model](doc/user-guide.md#model), a copy of the current row is
inserted into the `<model>_old` table before proceeding.  So there is
a full [`history`](doc/api.md#history) of the document's lifetime.

#### PostgreSQL 9.4 + JSON == NoSQL++

The whole point is that we are only using a few columns in the
PostgreSQL model tables, and just for management purposes: all the
goodies are in the JSON.  Needless to say it makes sense to keep the
objects you end up serializing to a specific model table pretty
consistent in their content...

However, I think it may well be practical to support referential
integrity, based just on the primary key column in different models.
So we should be able to support a *CAT owns one or more HUMANS*
relationship etc.  This is the point of using Postgres for JSON
documents: we can choose precisely how much of the old fashioned
relational database goodness to mix with the new fashioned NoSQL devil
may care hedonism...

#### lparallel support

There is (preliminary) [lparallel](http://lparallel.org) support in
the `postgres-json-parallel` system.  As at January 2015 you need a
bleeding edge bordeaux-threads to use it.  Do a

```
git clone https://github.com/sionescu/bordeaux-threads.git
```

in your quicklisp/local-projects directory, register and build, as
shown above.

## Tests

There is a test suite (radically incomplete) which relies upon the
lparallel support described above *so you cannot run the tests without
a recent bordeaux-threads*.  The same goes for the more informal tests
in [thread-test](tests/thread-test.lisp).

```
(ql:quickload :postgres-json-test)
(in-package :postgres-json-test)
(setf *postmodern-connection* '("mydb" "myuname" "" "mydbserver"))
(run-pgj-tests)
```

It would be nice to have this automated for cl-test-grid but how to
surmount the need for a working PostgreSQL 9.4 install?

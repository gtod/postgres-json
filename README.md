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

This library is **under development**. The interface is relatively
stable which is the positive way of saying it might still change.
There are a few other things to complete before an Alpha release.

## Documentation

* [Beginner's guide to JSON with Common Lisp]
(doc/beginners.md)
* [User's Guide](doc/user-guide.md)
* [API](doc/api.md)

Most of the library code has docstrings.

## Quickstart

#### Postgres

You will need a working PostgreSQL 9.4 install.  On Debian this is as
simple as `apt-get install postgresql-9.4`.  Try `pg_lsclusters` to
see what port your 9.4 install is on, if it is not 5432 you will
need to explicitly supply it as I have in the example below.

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

#### Postgres-JSON

At your REPL:

```
(ql:quickload :postmodern)
(pomo:connect-toplevel "mydb" "gtod" "" "localhost" :port 5433)
```

If you get a result from `(pomo:query "select 1")` you are ready
to go.

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

;; Once only operation, make a DB schema for all our models
(ensure-backend)

;; Once only operation, make a new model to store JSON docs on cats
(ensure-model 'cat)
```

In the output below I have elided some of the return values for
brevity.  `obj` is a trivial function to turn a list of pairs into a
hash table.  `pp-json` is a trivial function to pretty print a nested
lisp object of hash tables and sequences as JSON.

```common-lisp
(insert 'cat (obj "name" "joey" "coat" "tabby"))
1

(pp-json (fetch 'cat 1))
{
    "key":1,
    "coat":"tabby",
    "name":"joey"
}

(insert 'cat (obj "name" "max" "coat" "ginger"))

(insert 'cat (obj "name" "maud" "coat" "tortoiseshell"))

(keys 'cat)
(1 2 3)

(excise 'cat 2)
2

(keys 'cat)
(1 3)

(tally 'cat)
2

(update 'cat 3 (obj "name" "maud" "coat" "tortoiseshell" "age" 7
                      "likes" '("sunshine" 42)))
3

(pp-json (get 'cat 3))
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

See [simple](examples/simple.lisp) for similar code to the above.
There is an extended example in [human-1](examples/human-1.lisp) and
[human-2](examples/human-2.lisp).  If you do `(ql:quickload
:postgres-json-examples)` all examples will be compiled for you.

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

## Features

#### Immutability

The "immutability" part comes from the fact that when you
[`update`](doc/api.md#update) or [`excise`](doc/api.md/#excise) (which
means *delete* but is not a standard Common Lisp symbol) a [JSON
document](doc/user-guide.md#json-document) in a
[model](doc/user-guide.md#model), a copy of the current row is
inserted into the `<model>_old` table before proceeding.  So there is
a full [`history`](doc/api.md#history) of the object's lifetime.

#### PostgreSQL 9.4 + JSON == NoSQL++

The whole point is that we are only using a few columns in the
PostgreSQL model tables, and just for management purposes: all the
goodies are in the JSON.  Needless to say it makes sense to keep the
objects you end up serializing to a specific model table pretty
consistent in their content...

However, I think it may well be practical to support referential
integrity, based just on the primary key column in different models.
So we should be able to support a *CAT owns one or more HUMANS*
relationship etc.  This is the point of using Postgre for JSON
documents: we can choose precisely how much of the old fashioned
relational database goodness to mix with the new fashioned NoSQL devil
may care hedonism...

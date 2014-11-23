POSTGRES-JSON
===============

## Overview

A simple interface (and implementation) for immutable persistence of
Common Lisp objects (that can be JSON serialized) to a PostgreSQL 9.4+
database table using the jsonb type.

PostgreSQL 9.4 (in Beta as at late November 2014) is a rock solid
RDBMS which now also supports
[JSON types](http://www.postgresql.org/docs/9.4/static/datatype-json.html).
For many years Marijn Haverbeke's [Postmodern](http://marijnhaverbeke.nl/postmodern/)
has provided an excellent Common Lisp interface to PostgreSQL.

Common Lisp also has some good JSON libraries as compared by Sabra On
The Hill: [JSON libraries
comparison](https://sites.google.com/site/sabraonthehill/home/json-libraries).
[Yason](http://common-lisp.net/project/yason/) is a dependency of this project
but in fact you can use whatever Common Lisp JSON library you like.

Having spent some time working with the full blown NoSQL database
RethinkDB, thanks to the wonderful
[cl-rethinkdb](https://github.com/orthecreedence/cl-rethinkdb), I began
to wonder just how far one might get with the new JSON types in
Postgres.  This library represents a few steps in that direction.

## Status

This library is **under development**. The very simple interface of
the persistence model is probably stable and it compiles and seems to
work, but there is still much to do before an Alpha release.

## Documentation

Just about everything has a doc string, but that's more for
maintainers than users.  The interface you get per model is just five
functions for now, illustrated below.  Because the interface is
baked on demand (per model) there is no source code to bounce to for
your interface functions.  But you can do (in Emacs):
`slime-documentation` for say `cat:insert`.

## Quickstart

You will need a working PostgreSQL 9.4 beta install.  On Debian I
followed the instructions
[here](https://wiki.postgresql.org/wiki/Apt).  There is a FAQ on
getting the 9.4 beta which you should read to get the apt source line.
Since I already had an earlier install, Debian put the new PostgreSQL
cluster at port 5433 rather than 5432.  YMMV.

Make sure you can quickload Postmodern and connect to some database
with a form like:

`(pomo:connect-toplevel "mydb" "gtod" "" "localhost" :port 5433)`

If you now get a result from `(pomo:query "select 1")` you are ready
to go.

Clone this repo to sit under your `~/quicklisp/local-projects` and do

`(ql:quickload :postgres-json)`.

(Sometimes I need to delete
`~/quicklisp/local-projects/system-index.txt` for this to work).

Now evaluate these forms at the REPL:

```common-lisp
(defpackage :pj-test
  (:nicknames :pj-test)
  (:use :cl :postgres-json)
  (:import-from :postgres-json :obj :pp-json))

(in-package :pj-test)

;; Creates a new PostgreSQL (PG) schema called *db-schema*
(create-default-schema)

;; Creates a new PG sequence called *db-sequence*
(create-default-sequence)

;; Create the PG tables and indexes for our cat model
(create-backend cat)

;; Make a new lisp package with some exported functions for cats
(bake-interface cat)
```

Each of the preceeding are essentially "one time" operations.  All
models can certainly share the same PG schema and sequence, although
they do not have to (TODO).  Certainly we need only create the PG
backend for our model once.  `bake-interface` is a little more tricky,
it needs to be

```
(eval-when (:compile-toplevel :load-toplevel :execute)
  (bake-interface cat))
```

when included in source code files for compilation since (for better
or worse) it's making the lisp package `cat` dynamically, so needs to
be evaluated at compile time so you can write `(cat:get 44)` etc...

In the output below I have elided some of the return values for brevity.
`obj` is a trivial function to turn a list of pairs into a hash table.
`pp-json` is a trivial function to pretty print (thanks to yason) an
arbitrarily nested lisp object of hash tables and lists as JSON.

```common-lisp
PJ-TEST> (cat:insert (obj "name" "joey" "coat" "tabby"))
1
PJ-TEST> (pp-json (cat:get 1))
{
    "coat":"tabby",
    "name":"joey"
}
PJ-TEST> (cat:insert (obj "name" "max" "coat" "ginger"))
PJ-TEST> (cat:insert (obj "name" "maud" "coat" "tortoiseshell"))
PJ-TEST> (cat:keys)
(1 2 3)
PJ-TEST> (cat:delete 2)
2
PJ-TEST> (cat:keys)
(1 3)
PJ-TEST> (cat:update 3 (obj "name" "maud" "coat" "tortoiseshell" "age" 7
                            "likes" '("sunshine" 42)))
3
PJ-TEST> (pp-json (cat:get 3))
{
    "age":7,
    "coat":"tortoiseshell",
    "name":"maud",
    "likes":[
        "sunshine",
        42
    ]
}
```

We need a bulk insert, but still it's fun to do something like

```common-lisp
(dotimes (i 100)
  (cat:insert (obj "name" (format nil "maud-~A" i) "coat" "tortoiseshell" "age" 7
                   "likes" '("sunshine" 42))))

(pp-json (cat:get 77))
```

## Design

### What lisp objects can be serialized?

This depends on your choice of JSON library: If you have a way to
encode your lisp object (list, hash-table, array, CLOS objects, etc.)
to JSON you can now put it straight into PostgreSQL 9.4+.

For example: `(bake-interface cat :to-json jsown:to-json)`

### I do not want integer keys

This is not too hard.  You can supply a keyword argument `use-id` to
`insert` or (and this will take a little more effort, see
`bake-interface`) you could make a UUID sequence in PG and get values
from that.  TODO.

### Backend

The backend for a model cat (say) looks like

```sql
                                    Table "pgj_schema.cat"
   Column   |           Type           |                       Modifiers
------------+--------------------------+-------------------------------------------------------
 id         | integer                  | not null
 valid_to   | timestamp with time zone | not null default 'infinity'::timestamp with time zone
 valid_from | timestamp with time zone | not null default transaction_timestamp()
 jdoc       | jsonb                    | not null
Indexes:
    "cat_pkey" PRIMARY KEY, btree (id)
    "cat_gin" gin (jdoc)
```

```sql
            Table "pgj_schema.cat_old"
   Column   |           Type           | Modifiers
------------+--------------------------+-----------
 id         | integer                  | not null
 valid_to   | timestamp with time zone | not null
 valid_from | timestamp with time zone | not null
 jdoc       | jsonb                    | not null
Indexes:
    "cat_old_pkey" PRIMARY KEY, btree (id, valid_to)
    "cat_old_gin" gin (jdoc)
```

I suggest inserting some data and then playing around inside PSQL...

I have yet to determine just how leak proof the very simple CRUD
interface should be...

#### Immutability

The "immutability" part comes from the fact that when you use the
model to update or delete, we actually copy the current row to the
`_old` table before proceeding.  So we have a full history of the
object's lifetime.  TODO: write some nice interface functions for
this.

#### JSON == NoSQL

The whole point is that we are only using a few columns in the
PostgreSQL model tables, and just for management purposes: all the
goodies are in the JSON.  Needless to say it makes sense to keep the
objects you end up serializing to a specific model table pretty
consistent in their content...

However, I think it may well be practical to support referential
integrity, based just on the primary key id column in different
models.  So we should be able to support a "CAT owns one or more
HUMANS" relationship etc.  This is the point of using PostgreSQL for
JSON: we can choose precisely how much of the old fashioned database
goodness to go with the new fashioned JSON devil may care hedonism...

## Notes

Letters, numbers and dashes are OK in symbol names for PostgreSQL objects.
Don't try anything too funky besides.

All the Postmodern conditions will leak through this abstraction, at
present it is a pretty thin layer.  However because we are using the
PG *repeatable read isolation level* to safely insert and update two
tables at once work has been done to handle *serialization failures*
under the covers.  See the PG docs for more info on isolation levels.

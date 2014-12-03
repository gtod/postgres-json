POSTGRES-JSON
===============

## Overview

A simple interface for immutable persistence of Common Lisp objects
(that can be JSON serialized) to a PostgreSQL 9.4+ relation using the
jsonb type.

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

## Quickstart

You will need a working PostgreSQL 9.4 beta install.  On Debian I
followed the instructions at
https://wiki.postgresql.org/wiki/Apt.  There is a FAQ on
getting the 9.4 beta which you should read to get the apt source line.
Since I already had an earlier install, Debian put the new PostgreSQL
cluster at port 5433 rather than 5432.  YMMV.

Make sure you can quickload Postmodern and connect to some database
with a form like:

`(pomo:connect-toplevel "mydb" "myusername" "" "localhost" :port 5433)`

If you now get a result from `(pomo:query "select 1")` you are ready
to go.

Clone this repo to sit under your `~/quicklisp/local-projects` and do

```common-lisp
(ql:register-local-projects)
(ql:quickload :postgres-json)
```

Now evaluate these forms at the REPL:

```common-lisp
(defpackage :simple-1
  (:use :cl :postgres-json :postgres-json-model)
  (:shadowing-import-from :postgres-json-model :get :delete :count))

(in-package :simple-1)

;; Once only operation, make a DB schema for all our models
(create-backend)

;; Once only operation, make a new model to store JSON docs on cats
(create-model 'cat)
```

In the output below I have elided some of the return values for brevity.
`obj` is a trivial function to turn a list of pairs into a hash table.
`pp-json` is a trivial function to pretty print (thanks to yason) an
arbitrarily nested lisp object of hash tables and lists as JSON.

```common-lisp
> (insert 'cat (obj "name" "joey" "coat" "tabby"))
1
> (pp-json (get 'cat 1))
{
    "key":1,
    "coat":"tabby",
    "name":"joey"
}
> (insert 'cat (obj "name" "max" "coat" "ginger"))
> (insert 'cat (obj "name" "maud" "coat" "tortoiseshell"))
> (keys 'cat)
(1 2 3)
> (delete 'cat 2)
2
> (keys 'cat)
(1 3)
> (count 'cat)
2
> (update 'cat 3 (obj "name" "maud" "coat" "tortoiseshell" "age" 7
                      "likes" '("sunshine" 42)))
3
> (pp-json (get 'cat 3))
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

See [simple-1](examples/simple-1.lisp) for similar code to the above
you can compile and run.  [simple-2](examples/simple-2.lisp) is
similar but it does not shadow the common lisp symbols such as 'get
and 'delete.

Individual calls to a model function such as `insert` which write to
the DB get their own transaction.  But if you start a model
transaction yourself all model operations in the body occur within a
single transaction:

```common-lisp
> (log:config :debug)

> (dotimes (i 3)
    (insert 'cat (obj "name" (format nil "maud-~A" i))))

<DEBUG> [17:59:59] postgres-json  Starting transaction INSERT
<DEBUG> [17:59:59] postgres-json  Completing transaction INSERT
<DEBUG> [17:59:59] postgres-json  Starting transaction INSERT
<DEBUG> [17:59:59] postgres-json  Completing transaction INSERT
<DEBUG> [17:59:59] postgres-json  Starting transaction INSERT
<DEBUG> [17:59:59] postgres-json  Completing transaction INSERT

> (with-model-transaction (some-cats)
    (dotimes (i 3)
      (insert 'cat (obj "name" (format nil "maud-~A" i)))))

<DEBUG> [18:00:16] pj-test () - Starting transaction SOME-CATS
<DEBUG> [18:00:16] pj-test () - Completing transaction SOME-CATS
```

Note that to get rid of the debugging messages from log4cl just do
`(log:config :info)`.

## Documentation

The interface to a model is just a few functions for now, illustrated
above.  All the interface functions have comprehensive doc strings.
I find the easiest way to read them is by bouncing to say `insert`
with emacs M-. (ie. slime-edit-definition).

### User's guide (under construction)

#### Schema search paths

We don't have to worry too much about schema search paths because the
PG *qualified name* is baked into the model.  But you might want
to set them when playing in PSQL:

```sql
SET search_path TO <your_schema>, public;
```  

You can specify `to-json` for `insert` and `update` and `from-json`
for `get` at run time:

```common-lisp
PJ-TEST> (get 'cat 82 :from-json 'yason:parse)
#<HASH-TABLE :TEST EQUAL :COUNT 2 {100799D833}>
PJ-TEST> (pp-json (get 'cat 82 :from-json 'yason:parse))
{
    "coat":"rugged tortoiseshell",
    "name":"clementine"
}
```
#### PostgreSQL sequences

Now there are some good reasons for using just a single auto
incrementing sequence across **all** your models (for one thing, it
means that all your JSON documents have a unique key if you every need
to merge subsets from different models), but you can also have
one sequence per model:

```
(create-db-sequence 'foo)
(create-model 'dog (make-model-parameters 'dog :sequence 'foo))
```

## Design

### What lisp objects can be serialized?

This depends on your choice of JSON library: If you have a way to
encode your lisp object (list, hash-table, array, CLOS objects, etc.)
to JSON you can now put it straight into PostgreSQL 9.4+.

### I do not want integer keys

This is not too hard.  You can supply a keyword argument `use-key` to
`insert` or (and this will take a little more effort) you could make a
UUID sequence in PG and get values from that.  TODO.

### Backend

The backend for a model cat (say) looks like

```sql
                                    Table "pgj_schema.cat"
   Column   |           Type           |                       Modifiers
------------+--------------------------+-------------------------------------------------------
 key        | integer                  | not null
 valid_to   | timestamp with time zone | not null default 'infinity'::timestamp with time zone
 valid_from | timestamp with time zone | not null default transaction_timestamp()
 jdoc       | jsonb                    | not null
Indexes:
    "cat_pkey" PRIMARY KEY, btree (key)
    "cat_gin" gin (jdoc)
```

```sql
            Table "pgj_schema.cat_old"
   Column   |           Type           | Modifiers
------------+--------------------------+-----------
 key        | integer                  | not null
 valid_to   | timestamp with time zone | not null
 valid_from | timestamp with time zone | not null
 jdoc       | jsonb                    | not null
Indexes:
    "cat_old_pkey" PRIMARY KEY, btree (key, valid_to)
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

#### PostgreSQL 9.4 + JSON == NoSQL++

The whole point is that we are only using a few columns in the
PostgreSQL model tables, and just for management purposes: all the
goodies are in the JSON.  Needless to say it makes sense to keep the
objects you end up serializing to a specific model table pretty
consistent in their content...

However, I think it may well be practical to support referential
integrity, based just on the primary key column in different
models.  So we should be able to support a *CAT owns one or more
HUMANS* relationship etc.  This is the point of using PostgreSQL for
JSON: we can choose precisely how much of the old fashioned database
goodness to go with the new fashioned JSON devil may care hedonism...

#### Transaction isolation levels

See [transactions](postgres/transactions.lisp) for how `INSERT` and `UPDATE`
handle isolation levels using a retry loop.  We are not using the
default Postgres isolation level but rather `repeatable read`.  Do let
me know if you think it should be `serializable` and why, I am no
expert.

Also see project hermitage at https://github.com/ept/hermitage for
plenty of gory detail on isolation levels.

## Notes

Letters, numbers and dashes are OK in symbol names for PostgreSQL
objects.  Don't try anything too funky besides.

All the Postmodern conditions will leak through this abstraction, at
present it is a pretty thin layer.  However because we are using the
PG *repeatable read isolation level* to safely insert and update two
tables at once work has been done to handle *serialization failures*
under the covers.  See the PG docs for more info on isolation levels.

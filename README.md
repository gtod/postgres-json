POSTGRES-JSON
===============

'Model' in this context means "A simple interface (and
implementation) for immutable persistence of Common Lisp objects
(that can be JSON serialized) to a PostgreSQL 9.4+ database table
using the jsonb type.

## Notes

PostgreSQL 9.4+ supports columns of type jsonb in relations.  It would
be nice to have a simple Common Lisp API to such relations, where
there are just a fee coumns to support simple referential interity and
most of the datab proper is stashed in the jsonb column.

Also, we never actually update or delete a row --- we archive them in
another table, suffixed _old.

"PostgreSQL JSON persistence model" is the long name

Generically, we refer to `cat` as the *base table* and `cat_old` as the
*old table*.

Reference below to the "primary key in the base table" apply equally well the
the first part of the primary key in the old table.

Postmodern defprepared functions are suffixed with a $ symbol: get-cat$
But this are not part of the interface of the package

Before insert/update, we are dealing with lisp objects.  Upon insert/update
these are encoded to JSON.  Once fetch/got, they are parsed from JSON back to
lisp objects...

All symbols get the default s-sql:to-sql-name either implicityl or explicity to
become PostgreSQL names.  I'm using symbols (not keywords or strings) as that is
the Postmodern way...

Put model, base, old etc. inside a GLOSSARY

Do we need muffle warnings if we implement proper delete/drops?

Rethink naming of op functions below --- too close to the macro they call?

symobls dash go to underscores in PostgreSQL.  Don't
try anything else funck besides letters and dashes...

The DB stuff is called CREATE, the package stuff MAKE


;;; Whole thing assumes an open PG DB connection
;;; stash-id is expecting a hash table - document
;;; Test behaviour of nil, false, empty array etc on jsown:to-json
;;; Would be nice to lisply abstract the JSON select stuff from PG.
;;; of course, you can still go to the DB directly if you like.
;;; Make using Fkeys between tables easy...

## Todo

I could wrap the serialization failure condition and send a nice
message to the user...  Maybe I wrap *all* Postmodern conditions and
tell them the abstraction has leaked...?

unique ids from UUID example

Stash valid_to, valid_from in objects from get-all?

## Quickstart

To be concrete, let's say you have some JSON objects for your three cats:

```common-lisp
  {"name":"max", "coat":"Tortoiseshell"}
  {"name":"maud", "coat":"Ginger"}
  {"name":"ethelred", "coat":"Tabby"}
```

If we do: ...

then we now have a base table in postgres called `cat`
and an old table called `cat_old`.  We can now do

(cat:insert {"name":"max", "coat":"Tortoiseshell"})

(defpackage :test-postgresql-json
  (:nicknames :pgj)
  (:use :cl :postmodern :postgresql-json))

(in-package :pgj)

(connect-toplevel "cusoon" "gtod" "" "localhost")

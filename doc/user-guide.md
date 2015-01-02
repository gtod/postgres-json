Postgres-JSON User Guide
========================

## Introduction

If you are already proficient in Common Lisp and relational databases
and/or JSON you should find the [examples](../examples) and the [reference
documentation](api.md) sufficent to get going.  Otherwise the
[Beginner's Guide](beginners.md) and this document may help.

### Motivation

Consider the (edited) first JSON object in the JSON file
http://gtod.github.io/human.json:

```JSON
  {
    "guid": "d3129e77-9931-4fb1-bfca-d2a28bb641bf",
    "name": "Henry Gibson",
    "tags": [
      "nostrud",
      "et"
    ],
    "gender": "male",
    "age": 31,
    "favoriteFruit": "banana",
    "email": "henrygibson@kengen.com",
    "phone": "+1 (942) 411-3974",
    "address": "833 Gerald Court, Lewis, Virginia, 9307",
    "friends": [
      {
        "id": 0,
        "name": "Dena Marquez"
      }
    ],
    "isActive": false
  },
```

For some projects it makes sense to spend a lot of design time on data
dictionaries and table definitions in 3NF and other projects call for
just:

```common-lisp
  (create-model 'human)

  (with-input-from-file (stream *human-file*)
    (with-model-transaction ()
      (dolist (human (yason:parse stream))
        (insert 'human human))))
```

### Usage synposis

Call [`ensure-backend`](api.md#ensure-backend) just once.  Call
[`ensure-model`](api.md#ensure-model) on some models and then use the
various [model interface](api.md#model-interface) functions to
insert/update/fetch your JSON documents.

You can over-ride the default "one transaction per operation"
behaviour by wrapping a body of model interface calls in a
[`with-model-transaction`](api.md#model-and-database-interaction)
form.

Postgres-JSON never truly deletes a JSON document so you can view its
[`history`](api.md#history).

If you need more complex queries you can write them: [User defined
queries](#user-defined-json-queries).

## Terminology

### JSON terms

The [Beginner's Guide](beginners.md) has informal definitions and
http://www.json.org has formal defintions of the following terms:
*JSON object, JSON array, JSON value, JSON string, JSON number*.

##### JSON document

* Abstract DB: A *relation* has many *tuples*.
* Concrete DB: A *table* has may *rows*.
* Postgres-JSON: A *model* has many *JSON documents*.

A *JSON document* in the broadest sense is any piece of JSON text.  We
use it to mean a specific JSON text (typically using a *JSON object*
as it's *top level*) that resides in a specific *model*.

###### Homogeneous JSON documents

It is worth saying explicitly that there is nothing stopping you
creating some model `cat` and then stuffing *any JSON document you
like* into it:

```common-lisp
(create-model 'cat)
(insert 'cat 1)
(insert 'cat "Foo")
(insert 'cat (list 1 2 "Foo"))
(insert 'cat (obj "name" "Joey" "coat" "tabby" "age" 7))
```

This is the power (and the terror) of the NoSQL approach compared with
the traditional relational one.  Typically, however, you want to put
**only** cat like JSON documents into a cat model:

```common-lisp
(insert 'cat (obj "name" "Joey" "coat" "tabby" "age" 7))
(insert 'cat (obj "name" "max" "coat" "ginger"))
(insert 'cat (obj "name" "maud" "coat" "tortoiseshell"))
```
##### Top level

You can arbitrarily nest JSON structures (objects or arrays) but, by
the definition of nesting, there will be a root or *top level*
structure.  (In fact you can store strings and numbers directly in a
*model* but then you have no top level structure).  It is this
structure that the Postgres [existence operator]
(http://www.postgresql.org/docs/9.4/static/datatype-json.html#JSON-CONTAINMENT)
works on.

Here the [*properties*](#property) such as `"guid"` and `"company"`
are at the top level, but `"id"` is not:

```common-lisp
  {
    "guid": "d3129e77-9931-4fb1-bfca-d2a28bb641bf",
    "name": "Henry Gibson",
    "gender": "male",
    "friends": [
      {
        "id": 0,
        "name": "Dena Marquez"
      }
    ],
    "company": "KENGEN",
    "isActive": false
  },

```

##### JSON Serialization

The process of converting a Common Lisp *object* to a *JSON document*,
which is really just a string or stream of JSON.  Typically Lisp
objects are serialized to JSON before being inserted into a Postgres
*backend* *model*.

### Common Lisp terms

##### Object

When not explicitly qualifed by a "JSON" prefix, *object* is used in
the most general Common Lisp sense of "any common lisp datum".
Examples of Common Lisp objects include hash tables, vectors, strings
and numbers and in fact these are just the sort of objects ideal for
*JSON serialization*.

### Postgres-JSON terms

##### Backend

*Backend* describes a thin layer of abstraction over the Postgres
schema [`*pgj-schema*`](api.md#pgj-schema) in which all *models* are
created.  A Postgres schema is similar to a Common Lisp package in
that it provides a namespace for database tables etc.  All the [model
interface](api.md#model-interface) functions use the default schema
automatically.  But for [user defined
queries](#user-defined-json-queries) you must go to a little more
trouble.  The (trivial) backend functions are documented in
the API under [Postgres backend](api.md#postgres-backend).

##### Model

The term *model* (or *backend model*) serves to describe a thin
layer of abstraction over a pair of Postgres tables in which *JSON
documents* of a similar nature are stored.  It's a term best
understood when used concretely: "the cat model", "the human model"
etc.  Every model has the same *model interface*.

Specific models in Postgres-JSON are named by Common Lisp symbols.

##### Model interface

The set of Common Lisp functions such as `insert` and `fetch` that
provide a simple inteface to the underlying database operations on the
*JSON documents* of a specific *model*.  See the [model
interface](api.md#model-interface) API docs.

##### Model parameters

Parameters specific to a model, such as the Postgres data type of the
primary key field for example, are stored in an instance of CLOS class
[`model-parameters`](../model/parameters.lisp).  Because the creation
of a model and its use can be separated by large gaps of time we eat
our own dog food and serialize the model parameters to a backend
*model* called `*meta-model*`.  See [model
parameters](api.md#model-parameters).

##### Key

Typically used in the database sense of the *primary key* of a *JSON
document* stored in a specific Postgres-JSON *model*.  *Key* is often
short in Common Lisp for "hash table key" but we try and use
*property* for the second meaning.

##### Property

* Common Lisp: A *hash table* maps a *key* to a *value*.
* JavaScript: An *object* maps a *property* to a *value*.
* JSON: An *object* maps a *string* to a *value*.

Because we want to reserve the word *key* for use in the database
sense of *primary key*, and because *string* is too general, we use
the word *property* to describe the left hand side of a *JSON object*
string/value pair.  Note well, a property is **always a string**. The
following *JSON object* has the three properties `key`, `coat`,
`name`.

```common-lisp
{
    "key":1,
    "coat":"tabby",
    "name":"joey"
}
```

See, for example, [exists](api.md#exists).

## User defined JSON queries

These are documented as part of the [API]
(api.md#user-queries-and-json-syntactic-sugar-for-s-sql)
but some explanation and examples follow.

Because any given [*model*](#model) is just a thin layer over some
Postgres database tables, we can query them directly.  In some sense
this means our abstraction leaks but I'm not in the business of trying
to pretend SQL isn't a fine way to query a relational database...

[`define-json-query`](api.md#define-json-query) is a fairly light
wrapper over a standard Postmodern S-SQL query form.  If you are not
familair with S-SQL, read one or both of these:

* https://sites.google.com/site/sabraonthehill/postmodern-examples/postmodern-intro-to-s-sql#simple-queries
* https://marijnhaverbeke.nl/postmodern/s-sql.html).

What you get in addition is some syntactic sugar and the optional use
of parameter names in the prepared queries.

#### JSON query syntactic sugar

S-SQL largely supports the various JSON operators and does the right
thing when you use function syntax such as `(:json-build-object ...)`.

But the standard syntax becomes verbose with heavy use so
some more concise forms are defined in
[user-query.lisp](../model/user-query.lisp).  Everything else is still
S-SQL but any list starting with a symbol in the list below gets
special treatment when used with `define-json-query`:

* [`j->`](api.md#j-) returns a JSON object propery as JSON.
* [`j->>`](api.md#j--1) returns a JSON object propery as text.
* [`jbuild`](api.md#jbuild) returns a JSON object built out of JSON pieces on the database side.
* [`to-jsonb`](api.md#to-jsonb) casts an S-SQL form to Postgres type `jsonb`.

You can use the full model name such as `'cat` with these macros
(the quote is optional) but if in your `:from` or `:join` clause you
use an `:as` assignment then you can also use the assigned
symbol.

**Because they are all macros you can simply macroexpand them to see
what S-SQL they turn into**.  Do not evaluate them, they are not
Common Lisp.

```common-lisp
; sugar              ; S-SQL
(j-> "id")           (:-> 'jdoc "id")
(j-> 'cat "id")      (:-> 'cat.jdoc "id").
(j->> 'c "id")       (:->> 'c.jdoc "id").
```

```common-lisp
; sugar              ; S-SQL
(to-jsonb 1)         (:TYPE (:TO-JSON 1) JSONB)
```

```common-lisp
; sugar                       ; S-SQL
(jbuild ("id" "name"))        (:JSON-BUILD-OBJECT "id" (:-> 'JDOC "id") "name" (:-> 'JDOC "name"))
(jbuild ('cat "id" "name"))   (:JSON-BUILD-OBJECT "id" (:-> 'CAT.JDOC "id") "name" (:-> 'CAT.JDOC "name"))
```

```common-lisp
;; OK, no duplicated keys
(jbuild (cat "id" "name") (dog "age"))  ; No quotes actually needed

(:JSON-BUILD-OBJECT "id"  (:-> 'CAT.JDOC "id") "name" (:-> 'CAT.JDOC "name")
                    "age" (:-> 'DOG.JDOC "age"))
```

```common-lisp
;; Explicitly label duplicate key
(jbuild ('cat "id" "name") ('dog ("dog-id" "id") "age"))

(:JSON-BUILD-OBJECT "id"     (:-> 'CAT.JDOC "id") "name" (:-> 'CAT.JDOC "name")
                    "dog-id" (:-> 'DOG.JDOC "id") "age"  (:-> 'DOG.JDOC "age"))
```

`j->` and `j->>` only take one or two args. As shown above the table
name is optional, unless of course this would lead to ambiguity.

`to-jsonb` takes a single form as an argument, which will be converted
by the Postgres TO-JSON function and then cast to the Postgres `jsonb`
type.

`jbuild` takes 1 or more lists as args.  It is documented in the [API]
(api.md#user-queries-and-json-syntactic-sugar-for-s-sql) but the above
examples tell the full story.

#### JSON query named parameter interpolation

When using [`define-json-query`](api.md#define-json-query) you must
supply some **QUERY-PARAMS** for each of the parameters the query will
require at run time.  Here `min-balance` and `gender`:

```common-lisp
(define-json-query rich-humans$ (min-balance gender)
  (:order-by
   (:select (jbuild ("key" "guid" "gender" "name" "balance"))
    :from 'human
    :where (:and (:>= (:type (j->> "balance") real) min-balance)
                 (:= (j->> "gender") gender)))
   (:type (j->> "balance") real)))

(rich-humans$ 3900 "male")
```

You may write the symbols of the parameters list *inside the query
form itself*, instead of the standard S-SQL '$1, '$2 etc, as evidenced
above.

And, similar to
[`cl-ppcre:register-groups-bind`](http://weitz.de/cl-ppcre/#register-groups-bind),
you may write each parameter as a list, the first element of which is
a function designator to transform the actual arguments to the query
at run time.  See [`define-json-query`](api.md#define-json-query) and
the example below.

#### Examples

```common-lisp
;; We need filter arg as a JSON string, so request a funcall on *to-json* at run time
(define-json-query one-friend-humans$ ((*to-json* filter) email-regex)
  (:select 'jdoc ;; 'jdoc is the generic name for the JSON column in all models
   :from 'human

   ;; Our 'jdoc column is Postgres type jsonb.  j-> is sugar for
   ;; Postgres operator -> which returns a top level property in the
   ;; jdoc column as Postgres type jsonb.  Thus we must apply jsonb
   ;; functions to it...
   :where (:and (:or (:@> 'jdoc filter))                     ;; Postgres json containment
                (:= (:jsonb-array-length (j-> "friends")) 1) ;; Postgres jsonb function
                (:~ (j->> "email") email-regex))))           ;; Postgres regex operator
```

See [Empty JSON arrays and Common
Lisp](#empty-json-arrays-and-common-lisp) for further discusson
of the `jsonb-array-length` comparison above.

##### Joins

```common-lisp
(define-json-query uncharitable-humans$ ()
  (:select (jbuild (human "name") (gift "type" "quantity"))
   :from 'human
   :inner-join 'gift
   :on (:= (j-> human "key") (j-> gift "human-key"))
   :where (:= (j-> gift "quantity") (to-jsonb 1))))
```

We could also write the above `:where` clause as
```common-lisp
 :where (:= (:type (j->> gift "quantity") int4) 1)
```

`j->>` asks that gift `"quantity"` be converted to Postgres type
text, which we then cast to Postgres type int4 to compare with 1.

What we actually did is convert Postgres type integer 1 to Postgres
type jsonb using `(to-jsonb 1)`.  We then compared it with property
`"quantity"`, which is also Postgres type jsonb because Postgres
operator `->` gives the raw jsonb of the key while Postgres operator
`->>` converts it to text.  *The type of the `jdoc` column in all
models is Postgres jsonb.*

See also [Prepared queries data
types](../TODO.md#prepared-queries-data-types) in the TODO.

#### Search path shenanigans

We don't have to worry too much about schema search paths because the
Postgres *qualified name* is harcoded into [model based queries]
(api.md#model-interface) using [`*pgj-schema*`](api.md#pgj-schema).
But they are important for user defined queries because `:from 'cat`
(which is what we want to write) does not qualify the cat table, we
really need `:from 'pgj-model.cat`.  When using PSQL you can do:

```sql
SET search_path TO pgj_model, public;
```

so that `select * from cat` will just work.  For connections
from Common Lisp you can:

* Wrap connection calls in a macro such as this where
[`*default-search-path*`](api.md#default-search-path) is an exported
symbol of the Postgres-JSON package.  However there is a little
overhead to such a call:


```common-lisp
(defmacro with-pj-connection (() &body body)
  `(pomo:with-connection *connection*
     (pomo:set-search-path *default-search-path*)
     ,@body))
```

* Use
[`alter-role-set-search-path`](api.md#alter-role-set-search-path)
where you tell Postgres to use the specified search path for every
connection of a given user.

* Use the parallel version (courtesy of http://lparallel.org/) where
each worker has a persistent connection and the search path is set in
a manner similar to the above.  See
[threads-test](../tests/thread-test.lisp).

* Do anything else that works, such as hardcoding into `postgresql.conf`.
I considered writing another sugar macro so that `(qn cat)` became
`'pgj-model.cat` but did not want the syntax for user defined queries
to stray too far from S-SQL.  YMMV.

## Miscellaneous

#### Empty JSON arrays and Common Lisp

By default Yason will parse JSON arrays as Common Lisp lists and thus
an empty array becomes an empty list, which is CL `NIL`.  When we then
ask yason to serialize our empty list back to JSON we get JSON `null`:

```common-lisp
CL-USER> (yason:encode (yason:parse "[]"))
null
NIL
```

Now in order to write a Postgres query such as the following

```common-lisp
(:select 'jdoc
   :from 'human
   :where (:= (:jsonb-array-length (j-> "friends")) 1))
```

we need the values of the JSON object key `"friends"` to actually be an
array, rather than `null` when the person happens to be friendless.
So instead we ask yason to do

```
CL-USER> (yason:encode (yason:parse "[]" :json-arrays-as-vectors t))
[]
#()
```

which is what we want.  Of course, YMMV with other JSON libraries.
Yason is optional, you can use any JSON library you like, that is what
`*to-json*` and `*from-json*` are for, and the similar keyword
arguments to many interface functions.

#### PostgreSQL sequences

Now there are some good reasons for using just a single auto
incrementing sequence to generate the primary keys across **all** your
models (for one thing, it means that all your JSON documents have a
unique key if you every need to merge subsets from different models),
but you can also have one sequence per model if you need:

```
(create-db-sequence 'foo)
(create-model 'dog (make-model-parameters 'dog :sequence 'foo))
```

#### I do not want integer keys

This is not too hard.  Set the correct `key-type` in a call to
`make-model-parameters` and then use keyword argument `use-key` on
`insert`.  Or (and this will take a little more effort) you could make
a UUID sequence in PG and get values from that.  TODO.

#### Models in the Postgres backend

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

#### Transaction isolation levels

See [transactions](../postgres/transactions.lisp) for how
[`insert`](api.md#insert) and [`update`](api.md#update) handle
isolation levels using a retry loop.  We are not using the default
Postgres isolation level but rather `repeatable read`.  Do let me know
if you think it should be `serializable` and why, I am no expert.

Also see project hermitage at https://github.com/ept/hermitage for
plenty of gory detail on isolation levels.

#### Postmodern conditions

All the Postmodern conditions will leak through this abstraction, at
present it is a pretty thin layer.  However because we are using the
Postgres *repeatable read isolation level* to safely insert and update
two tables at once, work has been done to handle *serialization
failures* under the covers.

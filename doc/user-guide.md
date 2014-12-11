Postgres-JSON User Guide
========================

## Introduction

If you are already proficient in Common Lisp and relational databases
and/or JSON you may find the [examples](../examples) and the [reference
documentation](api.md) sufficent to get going.  Otherwise the
[Beginner's Guide](beginners.md) and this document may help.

### Conventions

In the code snippets I freely use
[Alexandria](http://common-lisp.net/project/alexandria/) functions and
macros.  Anything else you do not recognize will probably be an
exported symbol from [Postgres-JSON](../package.lisp).

### Motivation

Consider the first JSON object in the JSON file
http://gtod.github.io/human.json:

```JSON
  {
    "picture": "http://placehold.it/32x32",
    "guid": "d3129e77-9931-4fb1-bfca-d2a28bb641bf",
    "name": "Henry Gibson",
    "tags": [
      "nostrud",
      "sunt",
      "exercitation",
      "deserunt",
      "et"
    ],
    "gender": "male",
    "age": 31,
    "favoriteFruit": "banana",
    "registered": "2014-05-25T15:37:42 -10:00",
    "greeting": "Hello, Henry Gibson! You have 4 unread messages.",
    "longitude": -36.013046000000003,
    "email": "henrygibson@kengen.com",
    "phone": "+1 (942) 411-3974",
    "address": "833 Gerald Court, Lewis, Virginia, 9307",
    "latitude": -39.374197000000002,
    "balance": 1564.8499999999999,
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

## Usage

You `ensure-backend`, you `ensure-model` on some models and then
you use the various [model interface](api.md#model-interface) functions
to put stuff in and get stuff out of the backend database.

You can over-ride the default "one transaction per operation"
behaviour by wrapping a body of model interface calls in a
[`with-model-transaction`](api.md#model-and-database-interaction)
form.

Postgres-JSON never truly deletes a JSON document so you can view its
[`history`](api.md#history).

If you need more complex queries you can write them: [User defined
queries](#user-defined-json-queries).

### Model interface conventions

Any `KEY` arguments supplied to a model interface function must be
compatible with the Postgres data type KEY-TYPE, which is one of the
[model's parameters](api.md#model-parameters).  It defaults to
`integer`.

## Terminology and overview

Those proficient in Common Lisp and JSON can skip directly
to [Postgres-JSON terms](#postgres-json-terms).

It's difficult to avoid all forward references in this section.
Because I think it's easier to grasp the novel Postgres-JSON
terminology of *backend* and *model interface* having read about well
known things like JSON and Common Lisp objects, those terms come first.

### JSON terms

JSON is really simple which is, I suspect, why it has been so
successful.  [This page](http://www.json.org/) does a great job of
explaining it and parts of that page are repeated below.  It's worth
understanding a little JSON so that you can understand just what
Common Lisp *objects* you can sensibly serialize to JSON.

##### JSON Structure

JSON is built on two structures: the *JSON object* and *JSON array*.

##### JSON object

An unorded set of name/value pairs, where the name must be a *JSON
string* and the value a *JSON value*.

##### JSON array

An ordered collection of *JSON values*.

##### JSON value

A JSON value can be a *JSON string* in double quotes, or a *JSON
number*, or the barewords `true` or `false` or `null`, or a *JSON
object* or a *JSON array* (it's turtles all the way down).

##### JSON string

A JSON string is a sequence of zero or more Unicode characters,
wrapped in double quotes, using backslash escapes.

##### JSON number

A signed decimal number that may contain a fractional part and may use
exponential E notation. No distinction is made between integer and
floating-point. (Paraphrased from
[Wikipedia](http://en.wikipedia.org/wiki/JSON)).

##### JSON document

* Abstract DB: A *relation* cat has many *tuples*.
* Concrete DB: A *table* cat has may *rows*.
* Postgres-JSON: A *model* cat has many *JSON documents*.

A *JSON document* in the broadest sense is any piece of JSON text.  We
use it to mean a specific JSON text (typically using a *JSON object*
as it's *top level*) that resides in a specific *model*.

###### Homogeneous JSON documents

It may be obvious to you, but it's worth saying explicitly that there
is nothing stopping you creating some *model* `cat` and then stuffing
*any JSON document you like* into it:

```common-lisp
(create-model 'cat)
(insert 'cat 1)
(insert 'cat "Foo")
(insert 'cat (list 1 2 "Foo"))
(insert 'cat (obj "name" "Joey" "coat" "tabby" "age" 7))
```

This is the power (and the terror) of the NoSQL approach compared with
the traditional relational database approach.  Typically, however,
you want to put **only** cat like JSON documents into the cat model:

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

When not explicitly qualifed by a "JSON" prefix, *object* is typically
used in the most general Common Lisp sense (rather than the more
specific CLOS sense).  Examples of Common Lisp objects include hash
tables, vectors, strings and numbers --- just the sort of objects
ideal for *JSON serialization*.

### The Common Lisp/JSON connection

A Common Lisp hash table defined with `:test #'equal` and using
**only** string keys will serialize easily to a JSON object **as long
as the values of said hash are themselves serializable to JSON**.

A Common Lisp vector or list will serialize easily to JSON **as long
as the values of said vector or list are themselves serializable to
JSON**.

Common Lisp strings and numbers serialize easily to JSON.  Choices
have to be made about NIL --- see the [Beginner's guide]
(beginners.md).

So because JSON allows arbitrary nesting of its object and array
structures it becomes clear that *arbitrary nested combinations of the
above Common Lisp objects will also serialize easily to JSON.* For
example, a hash of hash of hashes with vector values of integers and
floats, with all hash keys being strings, will serialize easily to
JSON.

"Serialize easily" means there is a pretty natural mapping between the
Common lisp object in question and JSON.  Of course, with a little
effort and some simplifying assumptions you can serialize a CLOS
object (or anything else) to JSON: see for example [model parameters]
(../model/parameters.lisp) which makes a **lot** of simplifying
assumptions, or the features offered by
[CL-JSON](http://common-lisp.net/project/cl-json).

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

The term *model* (or *backend model*) serves to describe a (thin)
layer of abstraction over a pair of Postgres tables in which *JSON
documents* of a similar nature are stored.  It's a term best
understood when used concretely: "the cat model", "the human model"
etc.  Every model has the same *model interface*.

Specific models in Postgres-JSON are named by Common Lisp symbols.
The symbol is a single parameter that simply tells the [*model
interface*](api.md#model-interface) functions which Postgres tables
and other objects to use.

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
used in the Common Lisp sense "hash table key" but to avoid
overloading *key* too much we try and use *property* for the second
meaning, especially in the context of *JSON objects*, where unlike
Common Lisp hashes, properties must be strings.

##### Property

* Common Lisp: A *hash table* maps a *key* to a *value*.
* JavaScript: An *object* maps a *property* to a *value*.
* JSON: An *object* maps a *string* to a *value*.

Because we would like to reserve the word *key* primarily for use in
the database sense of *primary key*, and because *string* is far too
general, we choose to use the word *property* to describe the left
hand side of a *JSON object* string/value pair.  Note well, a property
is **always a string**. The following *JSON object* has the three
properties `key`, `coat`, `name`.

```common-lisp
{
    "key":1,
    "coat":"tabby",
    "name":"joey"
}
```

See, for example, [exists](api.md#exists).

## User defined JSON queries

*This section is too long, but I don't have time to make it shorter.
The [examples](../examples/human-2.lisp) and the [API
doc](api.md#user-queries-and-json-syntactic-sugar-for-s-sql) should
get you a long way)*.

Because any given [*model*](#model) is just a thin layer over some
Postgres database tables, we can query them directly.  In some sense
this means our abstraction leaks but I'm not in the business of trying
to pretend SQL isn't a fine way to query a relational database like
Postgres, even if our data is not in table columns but bundled up
inside a (potentially) complex JSON document which lives in a single
Postgres column of type `jsonb`.  It turns out you can get a long way
with the basic Postgres JSON facilities.

[`define-json-query`](api.md#define-json-query) is a fairly light
wrapper over a standard Postmodern S-SQL query form.  Iif you are not
familair with S-SQL, read one or both of these:

* https://sites.google.com/site/sabraonthehill/postmodern-examples/postmodern-intro-to-s-sql#simple-queries
* https://marijnhaverbeke.nl/postmodern/s-sql.html).

What you get in addition is some syntactic sugar and the optional use
of parameter names in the prepared queries.

#### Conventions

`define-json-query` prepares Postmodern queries that have a `:column`
result format and so you must ensure that each row produces just a
single datum, being a valid Postgres JSON type.  In practice this
means returning the column named `jdoc` in any model, which is the
entire JSON document, or using the [`jbuild`](api.md#jbuild) macro to
build some JSON on the fly.

#### JSON query syntactic sugar

S-SQL largely supports the various JSON operators and "does the right
thing" for function syntax such as `(:json-build-object ...)`, [see
the Postgres 9.4 JSON functions and operators
doc](http://www.postgresql.org/docs/9.4/static/functions-json.html)
for the full story, the following is somewhat simplified.

The standard syntax becomes a little verbose with heavy use so
some more concise forms are defined in
[user-query.lisp](../model/user-query.lisp).  Everything else is still
S-SQL but any list starting with a symbol in the list below gets
special treatment when used with `define-json-query`:

* [`j->`](api.md#j-) returns a JSON object propery as JSON.
* [`j->>`](api.md#j--1) returns a JSON object propery as text.
* [`jbuild`](api.md#jbuild) returns a JSON object built out of JSON pieces on the database side.
* [`to-jsonb`](api.md#to-jsonb) casts an S-SQL form to Postgres type `jsonb`.

You can use the full model name such as `'cat` with these j macros but
if in your `:from` or `:join` clause you have used an `:as` assignment
then you can also use the assigned symbol.  The quote on `'cat` or
`'c` is optional when using any of these j macros.

**Because they are all macros you can simply macroexpand them to see
what S-SQL they turn into**.  This may be the fastest way to learn.  Do
not evaluate them, they are not Common Lisp.

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
*The table name need not be quoted.  So `'cat` or `cat` are both fine.*

`to-jsonb` takes a single form as an argument, which will be converted
by the Postgres TO-JSON function and then cast to the Postgres `jsonb`
type.

`jbuild` takes 1 or more lists as args.  The syntax for each of these
list is as follows:

If the list starts with a symbol (or quoted symbol) then that is used
to qualify all the `jdoc` accesses for the following "keys" in the
list.  Keys may be strings in which case they do double duty as the
both the [*property*](#property) and the accessor.  A key may also be
a pair of strings in a list, the first being the property and the
second the accessor.  This flexibility is required because you are
building a *JSON object* and you cannot have duplicate properties (any
more than you can have duplicate keys in a hash table).  So if you
need the `"id"` property from both the `cat` and `dog` model, one of
them needs to be relabeled, as show in the examples above.  (This is
similar to how
[`with-slots`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_slts.htm#with-slots)
works).

#### JSON query named parameter interpolation

When using [`define-json-query`](api.md#define-json-query) you must
supply some **QUERY-PARAMS** which are `filter-json` and `email-regex` in
the example below:

```common-lisp
(define-json-query ready-bookings$ (filter-json email-regex)
  (:order-by
   (:select (jbuild ("id" "name" "email"))
    :from 'booking
    :where (:and (:or (:@> 'jdoc filter-json))
                 (:~ (j->> "email") email-regex)))
   (:type (j->> "price") real)))
```

Primarily these parameters become the parameters of the
`ready-bookings$` function defined by the macro.  But they have a
secondary (optional) use: you can simply use the same symbols inside
the query itself in place of the standard `'$1`, `'$2` etc. syntax of
S-SQL to specify the parameters that will be passed to the prepared
query on invocation.

In other words, you **must** provide explicit names for each parameter
as these become the parameters of the `ready-bookings$` function.  You
**may** write the params inline as shown above, or still write `'$1`,
`'$2` explicitly, as in standard S-SQL.

The
[*containment*](http://www.postgresql.org/docs/9.4/static/functions-json.html)
operator `@>` checks if some JSON you send as a query argument is
contained in the `jdoc` column of a specifc model.  Typically you
don't have JSON in the lisp program, you have say a hash-table which
needs to be serialized to JSON for this to work.  So rather than have
some variable `filter-json` which is actually a JSON string, we can
convert our lisp object on the fly by lifting a syntax idea from
[`cl-ppcre:register-groups-bind`](http://weitz.de/cl-ppcre/#register-groups-bind):
instead of writing a parameter as a symbol it can be written as a list
`(fn &rest params)` where `fn` is a function designator for a function
to map the actual arguments from themselves to something else: for
example `((*to-json* filter) email-regex)` funcalls `*to-json*` to map
`filter` from a hash-table to a JSON string when `ready-bookings$` is
called.

#### Examples

```common-lisp
(define-json-query rich-humans$ (min-balance gender)
  (:order-by

   ;; jbuild makes a new JSON object which consists of just the
   ;; key/values pairs in the JSON document with the specified properties.
   (:select (jbuild ("key" "guid" "gender" "name" "balance"))
    :from 'human

    ;; j->> is a little syntactic sugar to get at the top level properties
    ;; in 'jdoc using the Postgres operator :->> which returns text.
    ;; So we must cast to a Postgres number type for comparisons.
    :where (:and (:>= (:type (j->> "balance") real) min-balance)

                 (:= (j->> "gender") gender))) ;; can also do this using
                                               ;; 'containment', see below
   (:type (j->> "balance") real)))
```

```common-lisp
;; We need filter arg as a JSON string, so request a funcall on *to-json* at run time
(define-json-query one-friend-humans$ ((*to-json* filter) email-regex)
  (:select 'jdoc ;; 'jdoc is the generic name for the JSON column in all models
   :from 'human

   ;; Our 'jdoc column is Postgres type jsonb.
   ;; j-> is sugar for Postgres operator -> which returns a top level
   ;; property in jdoc as Postgres type jsonb.  Thus we apply jsonb
   ;; functions to it...
   :where (:and (:or (:@> 'jdoc filter))                     ;; Postgres json containment
                (:= (:jsonb-array-length (j-> "friends")) 1) ;; Postgres jsonb function
                (:~ (j->> "email") email-regex))))           ;; Postgres regex function
```

See [Empty JSON arrays and Common
Lisp](#empty-json-arrays-and-common-lisp) for further discusson
of the `jsonb-array-length` comparison above.

```common-lisp
;; Example of a join
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

which is what we did with `min-balance` in the `rich-humans$` query
above.  `j->>` asks that gift `"quantity"` be converted to Postgres type
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

#### Model's in the Postgres backend

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

#### Model names

Letters, numbers(?) and dashes are OK in symbol names for PostgreSQL
objects.  Don't try anything too funky besides.

#### Postmodern conditions

All the Postmodern conditions will leak through this abstraction, at
present it is a pretty thin layer.  However because we are using the
Postgres *repeatable read isolation level* to safely insert and update
two tables at once, work has been done to handle *serialization
failures* under the covers.

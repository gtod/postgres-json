Postgres-JSON User Guide
========================

## Introduction

If you are already proficient in Common Lisp and relational databases
and/or JSON you may find the [examples](../examples) and the reference
documentation (???)  sufficent to get going.  Otherwise the
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

## Features

Nothing is every really deleted, so it is trivial to see a full
history of updates made to a specific JSON document.

## Terminology

It's difficult to avoid all forward references in this section.
Because I think it's easier to grasp the novel Postgres-JSON
terminology of *backend* and *model interface* having read about well
know things like JSON and Common Lisp objects, those terms come first.

### JSON terms

JSON is really simple which is, I suspect, why it has been so
successful.  [This page](http://www.json.org/) does a great job of
explaining it and parts of that page are repeated below.  It's worth
understanding a little JSON so that you can understand just what
Common Lisp *objects* you can sensibly serialize to JSON.

#### JSON Structure

JSON is built on two structures: the *JSON object* and *JSON array*.

#### JSON object

An unorded set of name/value pairs, where the name must be a *JSON
string* and the value a *JSON value*.

#### JSON array

An ordered collection of JSON *values*.

#### JSON value

A JSON value can be a *JSON string* in double quotes, or a *JSON
number*, or the barewords `true` or `false` or `null`, or a *JSON
object* or a *JSON array* (it's turtles all the way down).

#### JSON string

A JSON string is a sequence of zero or more Unicode characters,
wrapped in double quotes, using backslash escapes.

#### JSON number

A signed decimal number that may contain a fractional part and may use
exponential E notation. No distinction is made between integer and
floating-point. (Paraphrased from
[Wikipedia](http://en.wikipedia.org/wiki/JSON)).

#### JSON document

* Abstract DB: A *relation* cat has many *tuples*.
* Concrete DB: A *table* cat has may *rows*.
* Postgres-JSON: A *model* cat has many *JSON documents*.

A *JSON document* in the broadest sense is any piece of JSON text.  We
use it to mean a specific JSON text (typically using a *JSON object*
as it's *top level*) that resides in a specific *model*.

##### Homogeneous JSON documents

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
#### Top level

You can arbitrarily nest JSON structures (objects or arrays) but, by
the definition of nesting, there will be a root or *top level*
structure.  (In fact you can store strings and numbers directly in a
*model* but then you have no top level structure).  It is this
structure that the Postgres [*existence* operator]
(http://www.postgresql.org/docs/9.4/static/datatype-json.html#JSON-CONTAINMENT)
works on.

Here the *properties* such as `"guid"` and `"company"` are at the top
level, but `"id"` is not:

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

#### JSON Serialization

The process of converting a Common Lisp *object* to a *JSON document*,
which is really just a string or stream of JSON.  Typically Lisp
objects are serialized to JSON before being inserted into a Postgres
*backend* *model*.

### Common Lisp terms

#### Object

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
(model/parameters.lisp) which makes a **lot** of simplifying
assumptions, or the features offered by
[CL-JSON](http://common-lisp.net/project/cl-json).

### Postgres-JSON terms

#### Backend

*Backend* describes a thin layer of abstraction over the Postgres
schema in which all *models* are created.  A Postgres schema is similar
to a Common Lisp
[package](http://www.lispworks.com/documentation/HyperSpec/Body/11_aa.htm)
in that it provides a namespace for database tables etc.  The backend
schema is called `pgj_model` --- see `*pgj-schema*` but you need
only concern yourself with this when using PSQL or defining your
own queries.  See [search path] and [define-json-query].

#### Model

The term *model* (or *backend model*) serves to describe a (thin)
layer of abstraction over a pair of Postgres tables in which *JSON
documents* of a similar nature are stored.  It's a term best
understood when used concretely: "the cat model", "the human model"
etc.  Every model has the same *model interface*.

**Specific models in Postgres-JSON are named by Common Lisp symbols.**
The symbol is a single parameter that simply tells the *model
interface* functions which Postgres tables and other objects to use.

#### Model interface

The set of Common Lisp functions such as `insert` and `fetch` that
provide a simple inteface to the underlying database operations on the
*JSON documents* of a specific *model*.

#### Model parameters

Parameters specific to a model, such as the Postgres data type of the
primary key field for example, are stored in an instance of CLOS class
[`model-parameters`](../model/parameters.lisp).  Because the creation
of a model and its use can be separated by large gaps of time we eat
our own dog food and serialize the model parameters to a backend
*model* called `*meta-model*`.

#### Key

Typically used in the database sense of the *primary key* of a *JSON
document* stored in a specific Postgres-JSON *model*.  *Key* is often
used in the Common Lisp sense "hash table key" but to avoid
overloading *key* too much we try and use *property* for the second
meaning, especially in the context of *JSON objects*, where unlike
Common Lisp hashes, properties must be strings.

#### Property

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

## Postgres-JSON backend interface

## Postgres-JSON model

### Basic interface

Any KEY arguments supplied to a model interface function must be
compatible with the Postgres data type KEY-TYPE, which is one of the
model's parameters.  It defaults to `integer`.  *Go to bigints? How
hard to upgrade to bigints live?*

### User defined JSON queries

;;; These must return JSON objects only.
;;; Macroexpand the define-json-query forms and the individual
;;; j->, j->>, jbuild and to-jsonb forms to see the generated S-SQL.
;;; See also the User Guide on User defined queries.

;;; These queries must return a single return JSON object per
;;; result row.  Hence jbuild and 'jdoc


#### JSON query syntactic sugar



;;; Try slime-edit-definition (M-.) on DEFINE-JSON-QUERY to read
;;; details on the syntactic sugar of j->, j->>, jbuild, to-jsonb and
;;; also the named parameter interpolation.


(define-json-query rich-humans$ (min-balance gender)
  (:order-by

   ;; jbuild makes a new JSON object which consists of just the
   ;; key/values pairs in the JSON document with the specified keys.
   (:select (jbuild ("key" "guid" "gender" "name" "balance"))

    :from 'human ;; this is why we need to set the search path above
                 ;; otherwise it would need to be 'pgj_model.history

    ;; j->> is a little syntactic sugar to get at the top level keys
    ;; in 'jdoc using the Postgres operator :->> which returns text.
    ;; So we must cast to a Postgres number type for comparisons.
    :where (:and (:>= (:type (j->> "balance") real) min-balance)

                 (:= (j->> "gender") gender))) ;; can also do this using 'containment', see below

   (:type (j->> "balance") real)))

;; We need filter arg as a JSON string, so request a funcall on *to-json* at run time
(define-json-query one-friend-humans$ ((*to-json* filter) email-regex)
  (:select 'jdoc ;; 'jdoc is the generic name for the JSON column in all models
   :from 'human

   ;; Our 'jdoc column is Postgres type jsonb.
   ;; j-> is sugar for Postgres operator -> which returns a top level
   ;; key in jdoc as Postgres type jsonb.  Thus we apply jsonb
   ;; functions to it...
   :where (:and (:or (:@> 'jdoc filter))                     ;; Postgres json 'containment' op
                (:= (:jsonb-array-length (j-> "friends")) 1) ;; Postgres jsonb function
                (:~ (j->> "email") email-regex))))           ;; Postgres regex function

;; Example of a join
(define-json-query uncharitable-humans$ ()
  (:select (jbuild (human "name") (gift "type" "quantity"))
   :from 'human
   :inner-join 'gift
   :on (:= (j-> human "key") (j-> gift "human-key"))
   :where (:= (j-> gift "quantity") (to-jsonb 1))

   ;; We could also write the above :where clause as
   ;; :where (:= (:type (j->> gift "quantity") int4) 1)
   ;; which is what we did with min-balance in the rich-humans$ query
   ;; above.  j->> asks that gift "quantity" be converted to Postgres
   ;; type text, which we then cast to Postgres type int4 to compare
   ;; with 1.

   ;; What we actually do is convert Postgres type integer 1 to
   ;; Postgres type jsonb using a little syntactic sugar --- try
   ;; macroexpanding the (to-jsonb 1) form .  We then compare compare
   ;; it with key "quantity", macroexpand (j-> gift "quantity"), which
   ;; is also Postgres type jsonb because Postgres operator -> gives
   ;; the raw jsonb of the key while Postgres operator ->> converts it
   ;; to text.  This library only uses Postgres type jsonb. See the
   ;; Postgres docs at
   ;; http://www.postgresql.org/docs/9.4/static/functions-json.html
   ))




## Putting it to work

### Postmodern and friends

This library is built on top of Marijn Haverbeke's
[Postmodern](http://marijnhaverbeke.nl/postmodern/).  Like that
library it is unashamedly PostgreSQL dependent.

[User defined queries](#user-defined-queries) are defined using
Postmodern's S-SQL with the sprinkly of just a little
[JSON query syntactic sugar](#json-query-syntactic-sugar).

### Package definition and imports

My own preference is for something like the following:

```common-lisp
(defpackage :simple-1
  (:use :cl :postgres-json :postgres-json-model)
  (:shadowing-import-from :postgres-json-model :get :delete :count))
```

The symbols in `postgres-json` are the usual fare and are unlikely to
cause clashes.  Those in `postgres-json-model` are the basic CRUD
operations on our persistence model and so have common names, some of
which must be shadowing imported.  Since I rarely use `cl:get`,
`cl:delete` or `cl:count` the above method is not too confusing for
me.  However if you prefer to have no such clashes you can do

```common-lisp
(defpackage :postgres-json-examples
  (:use :cl :postgres-json))
```

where now you must fully qualify access to the symbols
`postgres-json-model:get` etc., which is not too bad because that
package has a short nickname and you can write `pj:get`.

*In fact, before an initial release, it might be better to relabel
`get` as `fetch`, `delete` as `erase` and `count` as `total` (or
whatever) to avoid this issue all together.  Feedback welcome.*

### Postgres search path

We only need to set the search path for DEFINE-JSON-QUERY,
see below.  There is a small overhead for this, see
(alter-role-set-search-path) for a more permanent solution.

### Empty JSON arrays and Common Lisp

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

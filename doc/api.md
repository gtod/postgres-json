# Postgres-JSON Interface
* [Connections](#connections)
* [Model types](#model-types)
* [Basic model management](#basic-model-management)
* [Model CRUD generic functions](#model-crud-generic-functions)
* [Model transactions](#model-transactions)
* [JSON helper functions and specials](#json-helper-functions-and-specials)
* [User queries and JSON syntactic sugar for S-SQL](#user-queries-and-json-syntactic-sugar-for-s-sql)
* [Model customization generic functions](#model-customization-generic-functions)
* [Further model management](#further-model-management)
* [Postgres backend](#postgres-backend)
* [Postmodern isolation level transactions](#postmodern-isolation-level-transactions)
* [lparallel support (optional)](#lparallel-support-(optional))

---
## Connections
#### \*postmodern-connection\*
*Dynamic variable*

Set this to a list congruent with the parameters expected by
POSTMODERN:CONNECT-TOPLEVEL, for use by the testing and example
code.

#### ensure-top-level-connection
*Function*

```common-lisp
&optional (connect-spec *postmodern-connection*)
```

Ensure a Postmodern top level connection is active by applying the
contents of the list **CONNECT-SPEC** to POMO:CONNECT-TOPLEVEL.


---
## Model types
#### pgj-model
*Class*

The Postgres-JSON model base class supported by
implementation and interface methods for storing, querying and
modifying JSON documents in a Postgres database.

#### pgj-history-model
*Class*

A Postgres-JSON model that maintains a history of
previous values of updated or deleted documents.

#### pgj-object-model
*Class*

A Postgres-JSON model that consists of JSON
documents having an object root node.

#### pgj-history-object-model
*Class*

A Postgres-JSON model that maintains history and
consists of JSON documents having an object root node.


---
## Basic model management
#### define-global-model
*Macro*

```common-lisp
name constant (&rest superclasses)
```

Define a new class named **NAME**, a symbol, having **SUPERCLASSES**, all
symbols.  Define a global variable named **CONSTANT**, a symbol, with
value an instance of the new class.

#### ensure-backend
*Generic function*

```common-lisp
model
```

Call CREATE-BACKEND on **MODEL** unless said backend
already exists.

#### drop-backend
*Generic function*

```common-lisp
model
```

Drop the Postgres backend of **MODEL**.  This will
irrevocably delete all data associated with the model.


---
## Model CRUD generic functions
#### insert
*Generic function*

```common-lisp
model object &optional key
```

Insert lisp object **OBJECT** into the backend **MODEL**,
after JSON serialization.  If **KEY** is supplied use that as the primary
key for the JSON document rather than an automatically generated one.
Return the new primary key.

#### supersede
*Generic function*

```common-lisp
model key object
```

Replace the current value of the JSON document
having primary key **KEY** in **MODEL** with the JSON serialization of lisp
object **OBJECT**.  Return **KEY** on success, NIL if no such **KEY** is found.

#### supersede
*Method*

```common-lisp
(model pgj-history-model) key object
```

As per **SUPERSEDE** but keep a separate record of all previous rows.

#### fetch
*Generic function*

```common-lisp
model key
```

If there is a JSON document with primary key **KEY** in
**MODEL** return the result of deserializing it.  Otherwise return NIL.

#### fetch-all
*Generic function*

```common-lisp
model
```

Return as a list the result of deserializing all
JSON documents in **MODEL**.

#### excise
*Generic function*

```common-lisp
model key
```

Delete the JSON document with primary key **KEY** from
**MODEL**.  Return **KEY** on success, NIL if no such **KEY** exists.

#### excise
*Method*

```common-lisp
(model pgj-history-model) key
```

As per **EXCISE** but keep a separate record of all deleted rows.

#### excise-all
*Generic function*

```common-lisp
model
```

Delete all JSON documents in **MODEL**.  Returns the
number of documents deleted.

#### excise-all
*Method*

```common-lisp
(model pgj-history-model)
```

As per **EXCISE-ALL** but keep a separate record of all deleted rows.

#### keys
*Generic function*

```common-lisp
model
```

Return two values: a list of all primary keys for
**MODEL** and the length of that list.

#### tally
*Generic function*

```common-lisp
model
```

Return the count of all JSON documents in **MODEL**.

#### having-property
*Generic function*

```common-lisp
model property
```

Return the result of deserializing all JSON
documents in **MODEL** which have a top level object property **PROPERTY**, a
string, or if said string appears as an element of a top level array.
This is in the Postgres operator ?  sense.  Requires a Postgres GIN
index with operator class :jsonb-ops defined on **MODEL**.

#### enumerate-property
*Generic function*

```common-lisp
model property
```

Return all distinct values of the top level
**PROPERTY**, a string, in all of the JSON documents of **MODEL**.  JSON
deserialization is performed by funcalling \*FROM-JSON\*.  Note that
this is _not_ a prepared query so care must be taken that **PROPERTY** is
sanitized if it derives from arbitrary user input.

#### contains
*Generic function*

```common-lisp
model contains &key
```

Filter all JSON documents in **MODEL** by checking they
'contain', in the Postgres @> operator sense, the object **CONTAINS** which
will be serialized to a JSON document by funcalling \*TO-JSON\*.  If
**CONTAINS** is NIL, apply no containment restriction.

#### contains
*Method*

```common-lisp
(model pgj-object-model) contains &key properties limit
```

Filter all JSON documents in **MODEL** as follows.  Each document
must 'contain', in the Postgres @> operator sense, the object **CONTAINS**
which will be serialized to a JSON document by funcalling \*TO-JSON\*.
If **CONTAINS** is NIL, apply no containment restriction.  **PROPERTIES** may
be a list of strings being properties in the top level of the JSON
documents in **MODEL** and only the values of said properties will be
returned, bundled together in a JSON document.  If **PROPERTIES** is NIL
the entire JSON document will be returned.  **LIMIT**, if supplied, must
be an integer that represents the maximum number of objects that will
be returned.  If properties is NIL JSON deserialization is performed
by DESERILIZE, otherwise by funcalling \*FROM-JSON\*.  Note that this is
_not_ a prepared query so extra care must be taken if **PROPERTIES** or
CONTAIN derive from unsanitized user input.

#### history
*Generic function*

```common-lisp
model key &key
```

Return a list of the result of deserializing all
previous values of the JSON document with primary key **KEY** in **MODEL**.

#### history
*Method*

```common-lisp
(model pgj-history-model) key &key (validity-keys-p t) (valid-from-key "_validFrom") (valid-to-key "_validTo")
```

Return a list of the result of deserializing all previous values
of the JSON document with primary key **KEY** in **MODEL**, in chronological
order.  If **VALIDITY-KEYS-P** is true, include the 'valid_from' and
'valid_to' Postgres timestamps for the historical document as
properties in the top level JSON object --- it must be an object in
this case.  **VALID-FROM-KEY** and **VALID-TO-KEY** are strings that will be
the property names of the respective timestamps.


---
## Model transactions
#### with-model-transaction
*Macro*

```common-lisp
(&optional name) &body body
```

Evaluate **BODY** inside a Postgres transaction using the 'repeatable
read' isolation level in read/write mode.  Retry any serialization
failures although chronic incidence will still result in the client
seeing CL-POSTGRES-ERROR:SERIALIZATION-FAILURE conditions --- see also
\*SERIALIZATION-FAILURE-SLEEP-TIMES\*.  Implemented using Postmodern
WITH-LOGICAL-TRANSACTION so may be nested.  **NAME** can be used with
Postmodern's abort-transaction and commit-transaction. **NAME** should not
be a Postgres reserved word.  Ideal for any group of mutating model
interface functions.

#### rollback
*Function*

```common-lisp
name
```

If this is the root node of a nested set of WITH-MODEL-TRANSACTIONs
then 'rollback' the transaction **NAME**.  Otherwise rollback to the
Postgres savepoint **NAME**.

#### commit
*Function*

```common-lisp
name
```

If this is the root node of a nested set of WITH-MODEL-TRANSACTIONs
then 'commit' the transaction **NAME**.  Otherwise merely release the
savepoint **NAME**.

#### \*serialization-failure-sleep-times\*
*Dynamic variable*

```common-lisp
'(0 1 2 4 7)
```

The length of this list of real numbers determines the number of
times to retry when a Postgres transaction COMMIT sees a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE condition.  For each retry we
sleep the duration specified plus a random number of milliseconds
between 0 and 2000.  However, if 0 sleep is specified, we do not sleep
at all.  If set to NIL no condition handling is performed hence the
client will always see any such serialization failures.


---
## JSON helper functions and specials
#### obj
*Function*

```common-lisp
&rest args
```

Return an 'equal key/value hash-table consisting of pairs of **ARGS**.
For JSON use your keys must be Common Lisp strings.

#### pp-json
*Function*

```common-lisp
object &key (stream *standard-output*) (indent 4)
```

Pretty print lisp **OBJECT** as JSON to **STREAM** with specified **INDENT**.

#### \*to-json\*
*Dynamic variable*

```common-lisp
#'to-json
```

A function designator for a function of one argument which
serializes a lisp object to a JSON string.

#### \*from-json\*
*Dynamic variable*

```common-lisp
#'from-json
```

A function designator for a function of one argument which returns
the result of parsing the JSON string being its input.


---
## User queries and JSON syntactic sugar for S-SQL
#### define-json-query
*Macro*

```common-lisp
name (&rest query-params) &body query
```

Define a Postmodern S-SQL based **QUERY** with name **NAME**, a symbol.
**QUERY** may use the macro forms j->, j->> jbuild and to-json, documented
separately.  Elements of **QUERY-PARAMS** may be symbols, the number and
order of said symbols serving to define the parameters the query will
be supplied with at run time.  Additionally, any occurence of a symbol
from the **QUERY-PARAMS** list in the **QUERY** from proper will be replaced
with '$1, '$2 etc. as appropriate based on the order of **QUERY-PARAMS**.
In this way your queries may use named parameters, but this is not
mandatory.

Furthermore, a la `cl-ppcre:register-groups-bind`, any element of the
**QUERY-PARAMS** list may itself be a list of the form
(function-designator &rest params) in which case the PARAMS are
still treated as parameters, in order, but at run time
FUNCTION-DESIGNATOR is called on each of the actual arguments of the
PARAMS to transform said arguments before use by the underlying query.
For example `(foo (\*to-json\* bar baz) blot)` is an acceptable
**QUERY-PARAMS** list, as long as \*to-json\* is funcallable.  bar and baz
will be replaced by the result of funcalling \*to-json\* on them,
repectively.

The Postmodern result format is always `:column` and so you must
ensure that each row produces just a single datum, being a valid
Postgres JSON type.  In practice this means either i) returning the
column named `jdoc` in any model, which is the entire JSON document,
or ii) using the `jbuild` macro to build a JSON object on the fly.

#### j->
*Macro*

```common-lisp
form1 &optional form2
```

S-SQL syntactic sugar to turn a single string **FORM1** into a
Postgres -> operation using the default JSON column 'jdoc and the
property FORM1; or to turn a symbol **FORM1** and string **FORM2** into a ->
operation using the 'jdoc JSON column in table **FORM1** and the property
**FORM2**.

#### j->>
*Macro*

```common-lisp
form1 &optional form2
```

S-SQL syntactic sugar to turn a single string **FORM1** into a Postgres
->> operation using the default JSON column 'jdoc and the property
FORM1; or to turn a symbol **FORM1** and string **FORM2** into a ->> operation
using the 'jdoc JSON column in table **FORM1** and the property **FORM2**.

#### to-jsonb
*Macro*

```common-lisp
form
```

S-SQL syntactic sugar to cast **FORM** to the Postgres jsonb type.

#### jbuild
*Macro*

```common-lisp
&rest key-forms
```

S-SQL syntactic sugar to create a new Postgres JSON object from the
**KEY-FORMS**.  Each KEY-FORM is a list.  In the simplest and first case
it may be a list of strings, said strings indicating properties of the
top level JSON object in the 'jdoc column of the query; the properties
and their values will be returned by **JBUILD**, in a fresh JSON object.
In the second case the list may start with a symbol (or a quoted
symbol) in which case the following strings indicate properties of
the top level JSON document in the 'jdoc column in the DB table named
by the symbol.  Now, a la `with-slots`, each string in the list may
itself be replaced by a list of two strings, the first being the
resulting property name in the object returned by **JBUILD**, the second
being the accessor property for the top level JSON object in the 'jdoc
column.  This flexibility is required because we are building a JSON
object and cannot have duplicate properties so if we need the "id"
property from both a `cat` and a `dog` model, one of them needs to be
relabeled.


---
## Model customization generic functions
#### model-sequence
*Generic function*

```common-lisp
model
```

The name, a symbol, of a Postgres sequence to
provide primary keys upon insertion of fresh documents into a backend
model.  May be NIL, in which case explicit primary keys must be
supplied for all inserts.

#### model-key-name
*Generic function*

```common-lisp
model
```

The name, a symbol, for the primary key column in
backend model tables.

#### model-key-type
*Generic function*

```common-lisp
model
```

The name, a symbol, for the Postgres type of the
primary key column in the backend model tables.  KEY arguments to
model interface methods must be compatible with this type.

#### model-initial-gin-operator-class
*Generic function*

```common-lisp
model
```

The name, a keyword, for the initial Postgres GIN
operator class to use for the model's GIN index.  See also
USE-GIN-INDEX.  If NIL, make no GIN index.

#### serialize
*Generic function*

```common-lisp
model object
```

Serialize lisp **OBJECT** to a form suitable for
storage as a JSON document in backend **MODEL**.  Return same.  Called by
INSERT, for example, to convert Lisp objects to JSON before DB
insertion proper.

#### deserialize
*Generic function*

```common-lisp
model jdoc
```

Deserialize the string **JDOC** from **MODEL**'s backend to
a lisp object.  Return same.  Called by FETCH, for example, to convert
JSON strings from the backend into Lisp objects.

#### stash
*Method*

```common-lisp
(model pgj-object-model) (object map) key
```

Add a key named by the downcased symbol name of MODEL-KEY-NAME of
**MODEL**, with value **KEY**, to the map **OBJECT**.  Return the new map.

#### stash
*Generic function*

```common-lisp
model object key
```

Called before SERIALIZE which is called before
document inserts or updates.  An opportunity to modify the lisp **OBJECT**
using the intended/current primary **KEY** of the JSON document in the
**MODEL**'s backend.

#### stash
*Method*

```common-lisp
(model pgj-model) object key
```

Do nothing and return **OBJECT**.

#### stash
*Method*

```common-lisp
(model pgj-object-model) (object hash-table) key
```

Destructively modify hash-table **OBJECT** by assigning the value **KEY**
to a key named by the downcased symbol name of MODEL-KEY-NAME of
**MODEL**.  Returns the modified **OBJECT**.


---
## Further model management
#### create-backend
*Generic function*

```common-lisp
model
```

Create the backend tables and indexes for a
**MODEL**.

#### backend-exists-p
*Generic function*

```common-lisp
model
```

Return true if **MODEL** has a Postgres backend, NIL
otherwise.

#### database-safety-net
*Condition*

Signaled to prevent accidental deletion of database
assets such as tables or schema.

#### really-do-it
*Function*

```common-lisp
condition
```

Invoke a '**REALLY-DO-IT** restart.

#### \*gin-operator-classes\*
*Dynamic variable*

```common-lisp
'(:jsonb-ops :jsonb-path-ops)
```

A list of keywords representing Postgres GIN operator classes.

#### use-gin-index
*Generic function*

```common-lisp
model gin-operator-class
```

Create a Postgres GIN index for **MODEL** using
**GIN-OPERATOR-CLASS**, a keyword that must be a member of
\*gin-operator-classes\*.  First drop any existing GIN index.


---
## Postgres backend
#### \*pgj-schema\*
*Dynamic variable*

```common-lisp
'pgj-model
```

A symbol being the name of the Postgres schema created to house all
database backend objects.

#### drop-pgj-schema
*Function*

Drop the entire Postgres schema \*PGJ-SCHEMA\* in the database
Postmodern is currently connected to.  This will irrevocably delete
ALL your data in ALL your models so it uses a RESTART-CASE to guard
against human error.

#### \*default-search-path\*
*Dynamic variable*

```common-lisp
(format nil "~A,public" (to-sql-name *pgj-schema*))
```

The default value used by ALTER-ROLE-SET-SEARCH-PATH.

#### alter-role-set-search-path
*Function*

```common-lisp
user &optional (search-path *default-search-path*)
```

Alter the role of Postgres user **USER**, a string, to set the
'search_path' setting to the string **SEARCH-PATH**.  In most cases this
is what you want so than when defining your own queries with
DEFINE-MODEL-QUERY unqualified relation names can be found in our
default schema (which is not the PUBLIC schema).  This setting does
_not_ effect the normal model interface functions such as FETCH and
FILTER as they use fully qualified table names at all times.  Will
only take effect upon your next connection.  Beware, may be overridden
by settings in your ~/.psqlrc file.  See also the Postgres
documentation on search paths and settings.

#### create-db-sequence
*Function*

```common-lisp
sequence &optional (schema *pgj-schema*)
```

Create a PostgreSQL sequence with name **SEQUENCE** in **SCHEMA** (both symbols).
Requires an active DB connection.

#### flush-prepared-queries
*Function*

If you get a 'Database error 26000: prepared statement ... does not
exist error' while mucking around at the REPL, call this.  A similar
error in production code should be investigated.


---
## Postmodern isolation level transactions
#### \*pgj-default-isolation-level\*
*Dynamic variable*

```common-lisp
'+repeatable-read-rw+
```

The isolation level, a symbol, to use for WITH-MODEL-TRANSACTION.
For models that maintain history can only be +REPEATABLE-READ-RW+ or
+SERIALIZABLE-RW+.  For models without history could conceivably be
+READ-COMMITTED-RW+.

#### incompatible-transaction-setting
*Condition*

Signaled for a nested invocation of
WITH-ENSURED-TRANSACTION-LEVEL or WITH-LOGICAL-TRANSACTION-LEVEL
inside a previous invocation with an incongruent isolation level.

#### +serializable-rw+
*Constant*

```common-lisp
"isolation level serializable read write"
```

START TRANSACTION string to set Postgres
'Serializable' isolation level and read/write.

#### +repeatable-read-rw+
*Constant*

```common-lisp
"isolation level repeatable read read write"
```

START TRANSACTION string to set Postgres 'Repeatable
read' isolation level and read/write.

#### +read-committed-rw+
*Constant*

```common-lisp
"isolation level read committed read write"
```

START TRANSACTION string to set Postgres 'Read
committed' isolation level, which is the default, and read write.

#### +read-committed-ro+
*Constant*

```common-lisp
"isolation level read committed read only"
```

START TRANSACTION string to set Postgres 'Read
committed' isolation level, which is the default, and read only.

#### with-transaction-level
*Macro*

```common-lisp
(name isolation-level) &body body
```

Unilaterally evaluate **BODY** inside a Postmodern WITH-TRANSACTION
form with Postgres 'transaction mode' set to the symbol-value of
**ISOLATION-LEVEL**, a symbol.  The symbol **NAME** is bound to the Postmodern
`transaction-handle' and may be used in calls to Postmodern's
abort-transaction and commit-transaction.

#### with-logical-transaction-level
*Macro*

```common-lisp
(name isolation-level) &body body
```

Similar to Postmodern's WITH-LOGICAL-TRANSACTION but start any top
level transaction with Postgres 'transaction mode' set to the
symbol-value of **ISOLATION-LEVEL**.  The symbol **NAME** is bound to the
Postmodern `transaction-handle' and may be used in calls to
Postmodern's abort-transaction and commit-transaction.  The condition
`incompatible-transaction-setting' will be signaled for incongruent
nested isolation levels.

#### ensure-transaction-level
*Macro*

```common-lisp
(isolation-level) &body body
```

Similar to Postmodern's ENSURE-TRANSACTION but start any top level
transaction with Postgres 'transaction mode' set to the symbol-value
of **ISOLATION-LEVEL**.  The condition `incompatible-transaction-setting'
will be signaled for incongruent nested isolation levels.


---
## lparallel support (optional)
#### \*pgj-kernel\*
*Dynamic variable*

An lparallel kernel to manage worker threads.  Typically bound to
the result of MAKE-PGJ-KERNEL for use by interface calls such
WITH-CONNECTED-THREAD.

#### \*pgj-database\*
*Dynamic variable*

Thread local Postmodern database connection.

#### make-pgj-kernel
*Function*

```common-lisp
connect-spec &optional (n 4)
```

Make an lparallel kernel object where each worker thread is given a
permanent DB connection, made using a Postmodern **CONNECT-SPEC**, a list.
Start **N** workers.  Ensure your Postgres can handle at least **N**
concurrent connecions.

#### end-pgj-kernel
*Function*

End the lparallel kernel in \*PGJ-KERNEL\*.

#### call-with-connected-thread
*Function*

```common-lisp
function
```

Ask that an lparallel worker perform **FUNCTION**, a function, given a
current Postmodern DB connection.  Block until the result is received
and return it.  \*PGJ-KERNEL\* must be bound to the result of
MAKE-PGJ-KERNEL.

#### with-connected-thread
*Macro*

```common-lisp
nil &body body
```

Wrap **BODY** in a lambda and invoke CALL-WITH-CONNECTED-THREAD.
\*PGJ-KERNEL\* must be bound to the result of MAKE-PGJ-KERNEL.

#### \*pgj-channel\*
*Dynamic variable*

A single lparallel channel for submitting tasks via SUBMIT-PGJ-TASK
and receiving results via RECEIVE-PGJ-RESULT.

#### make-pgj-channel
*Function*

Make an lparallel channel.  \*PGJ-KERNEL\* must be bound to the
result of MAKE-PGJ-KERNEL.

#### submit-pgj-function
*Function*

```common-lisp
function
```

Submit the function **FUNCTION**, with a Postmodern connection, as an
lparallel task on our channel \*PGJ-CHANNEL\*.  \*PGJ-KERNEL\* must be
bound to the result of MAKE-PGJ-KERNEL.

#### submit-pgj-task
*Macro*

```common-lisp
nil &body body
```

Wrap **BODY** in a lambda and call SUBMIT-PGJ-FUNCTION.
\*PGJ-KERNEL\* must be bound to the result of MAKE-PGJ-KERNEL.

#### receive-pgj-result
*Function*

Call lparallel:receive-result on our \*PGJ-CHANNEL\*.
\*PGJ-KERNEL\* must be bound to the result of MAKE-PGJ-KERNEL.

#### try-receive-pgj-result
*Function*

```common-lisp
&key timeout
```

Call lparallel:try-receive-result on our \*PGJ-CHANNEL\*,
with timeout **TIMEOUT**, a real.  \*PGJ-KERNEL\* must be bound to the
result of MAKE-PGJ-KERNEL.


---

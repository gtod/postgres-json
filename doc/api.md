# Postgres-JSON Interface
* [Postgres backend](#postgres-backend)
* [Model creation and management](#model-creation-and-management)
* [Model interface](#model-interface)
* [Model and database interaction](#model-and-database-interaction)
* [User queries and JSON syntactic sugar for S-SQL](#user-queries-and-json-syntactic-sugar-for-s-sql)
* [Model parameters](#model-parameters)
* [Trival helper functions](#trival-helper-functions)
* [Miscellaneous backend functions](#miscellaneous-backend-functions)
* [Specials](#specials)

---
## Postgres backend
#### CREATE-BACKEND
*Function*

Create the schema \*PGJ-SCHEMA\* and other backend objects needed to
house user created PostgreSQL JSON persistence models.  Call just once
in a given PostgreSQL database.

#### BACKEND-EXISTS-P
*Function*

Does the backend \*PGJ-SCHEMA\* exist?

#### ENSURE-BACKEND
*Function*

Call CREATE-BACKEND unless the Postgres backend already exists.

#### DROP-BACKEND
*Function*

Drop the backend (that is the PostgreSQL schema \*PGJ-SCHEMA\*) in
the database Postmodern is currently connected to.  This will
irrevocably delete ALL your data in ALL your models so it uses
a RESTART-CASE to guard against human error.


---
## Model creation and management
#### CREATE-MODEL
*Function*

```common-lisp
model &optional (parameters (make-model-parameters model))
```

Create the PostgreSQL tables and indexes for PostgreSQL JSON
persistence model **MODEL**, a symbol.  Uses the various values in the
**PARAMETERS** has table to customize the model.  Should only be called
once per model.  Returns **MODEL**.

#### MODEL-EXISTS-P
*Function*

```common-lisp
model
```

Does **MODEL**, a symbol, exist in our backend?

#### ENSURE-MODEL
*Function*

```common-lisp
model &optional (parameters (make-model-parameters model))
```

Call CREATE-MODEL with **MODEL** and **PARAMETERS** as arguments, unless **MODEL**
already exists.

#### DROP-MODEL
*Function*

```common-lisp
model
```

Drop model **MODEL**.  This will irrevocably delete all data associated
with the model so it uses a RESTART-CASE to guard against human
error.

#### ALL-MODELS
*Function*

Return a list of all models in the backend.


---
## Model interface
#### INSERT
*Function*

```common-lisp
model object &key use-key (stash-key *stash-key*) (to-json *to-json*)
```

Insert lisp object **OBJECT** into the backend **MODEL**, a symbol,
after JSON serialization.  If **USE-KEY** is supplied, use that as the
primary key for the JSON document rather than the automatically
generated one.  If **STASH-KEY** is non null we FUNCALL it with two
arguments: the value of the key to be used for the DB insert and
**OBJECT**.  It should return an object which will be inserted in the
place of the original.  Typically you would use this to 'stash' the
fresh primary key inside your object before serialization.  **TO-JSON**
must be a function designator for a function of one argument to
serialize lisp objects to JSON strings.  Return the new primary key.

#### UPDATE
*Function*

```common-lisp
model key object &key (stash-key *stash-key*) (to-json *to-json*)
```

Update the current value of the JSON document with primary key **KEY**,
in backend **MODEL**, a symbol, to be the JSON serialization of lisp
object **OBJECT**.  If **STASH-KEY** is non null we FUNCALL it with two
arguments: the value of the key to be used for the DB insert and
**OBJECT**.  It should return an object which will be used in the place of
the original.  **TO-JSON** must be a function designator for a function of
one argument to serialize lisp objects to JSON strings.  Return **KEY**
on success, NIL if there was no such **KEY** found.

#### FETCH
*Function*

```common-lisp
model key &key (from-json *from-json*)
```

Lookup the JSON document with primary key **KEY** in **MODEL**, a symbol.
If such a document exists return a parse of it by the function of one
argument designated by **FROM-JSON** (make it #'IDENTITY to return just
the JSON string proper).  If the JSON document does not exist, return
NIL.

#### FETCH-ALL
*Function*

```common-lisp
model &key (from-json *from-json*)
```

Return a list of all JSON documents in **MODEL**, a symbol, after
parsing by the the function of one argument designated by **FROM-JSON**.

#### EXCISE
*Function*

```common-lisp
model key
```

Delete the JSON document with primary key **KEY** from **MODEL**, a symbol.
Return **KEY** on success, NIL if there was no such **KEY** found.

#### EXCISE-ALL
*Function*

```common-lisp
model
```

Delete all JSON documents in **MODEL**, a symbol.  In fact this is a
recoverable operation in a sense as all deleted rows will still be in
the <model>-old Postgres relation.

#### KEYS
*Function*

```common-lisp
model
```

Returns two values: a list of all primary keys for this **MODEL**, a
symbol, and the length of that list.

#### TALLY
*Function*

```common-lisp
model
```

Return a count of JSON documents in **MODEL**, a symbol.

#### FILTER
*Function*

```common-lisp
model &key contain properties limit (to-json *to-json*) (from-json *from-json*)
```

Filter all JSON documents in **MODEL**, a symbol as follows.  Each
document must 'contain', in the Postgres @> operator sense, the object
**CONTAIN** which itself must serialize to a JSON document.  If **CONTAIN** is
NIL, apply no containment restriction.  **PROPERTIES** may be a list of
strings being properties in the top level objects of the JSON
documents in **MODEL** and only the values of said properties will be
returned, bundled together in a JSON document.  If **PROPERTIES** is NIL
the entire JSON document will be returned.  **LIMIT**, if supplied, must
be an integer that represents the maximum number of objects that will
be returned.  **CONTAIN** will be JSON serialized by **TO-JSON**, a function
designator for a function of one argument.  The returned JSON
documents will be parsed by the function of one argument designated by
**FROM-JSON**.  Note that this is _not_ a prepared query so extra care
must be taken if **PROPERTIES** or **CONTAIN** derive from unsanitized user
input.

#### EXISTS
*Function*

```common-lisp
model property &key (from-json *from-json*)
```

Return all JSON documents in **MODEL**, a symbol, which have a top
level object property **PROPERTY**, a string, or if said string appears as
an element of a top level array.  This is in the Postgres operator ?
sense.  The returned JSON documents will be parsed by the function of
one argument designated by **FROM-JSON**.  Requires a Postgres GIN index
without JSONB_PATH_OPS defined on **MODEL**.

#### DISTINCT
*Function*

```common-lisp
model property &key (from-json *from-json*)
```

Return all distinct values of the top level **PROPERTY**, a string, in
all of the JSON documents of **MODEL**, a symbol.  Every JSON document
must be a JSON object, with property **PROPERTY** defined.  So this
**DISTINCT** does not make sense if your JSON documents are arrays.  The
returned JSON documents wil parsed by the function of one argument
designated by **FROM-JSON**.  Note that this is _not_ a prepared query so
care must be taken that **PROPERTY** is sanitized if it derives from
arbitrary user input.

#### HISTORY
*Function*

```common-lisp
model key &key (from-json *from-json*) (validity-keys-p t) (valid-from-key "_validFrom") (valid-to-key "_validTo")
```

Returns a list, in chronological order, of all previous values of
the JSON document with primary key **KEY** in **MODEL**, a symbol.  If such
documents exist return a parse of each JSON string by the function of
one argument designated by **FROM-JSON**.  If the document has no history,
return NIL.  If **VALIDITY-KEYS-P** is true, include the 'valid_from' and
'valid_to' Postgres timestamps for the historical document as
properties in the top level JSON object --- it must be an object in
this case.  **VALID-FROM-KEY** and **VALID-TO-KEY** are strings that will be
the property names of the respective timestamps.


---
## Model and database interaction
#### WITH-MODEL-TRANSACTION
*Macro*

```common-lisp
(&optional (name 'user)) &body body
```

Evaluate **BODY** inside a Postgres transaction using the 'repeatable
read' isolation level.  Nested expansions of this macro or other
transaction macros used internally by POSTGRES-JSON are merely logical
within **BODY** --- they do not make real Postgres transactions.  So this
macro is designed for bulk inserts or when Atomicity of multiple
inserts/updates/deletes is required.


---
## User queries and JSON syntactic sugar for S-SQL
#### DEFINE-JSON-QUERY
*Macro*

```common-lisp
name (&rest query-params) &body query
```

Define a Postmodern S-SQL based **QUERY** with name **NAME**, a symbol,
using both the 'JSON query named parameter interpolation' for each
symbol in the list **QUERY-PARAMS** and the 'JSON query syntactic sugar',
both documented in the User Guide.  In fact, elements of **QUERY-PARAMS**
may be lists of the form (function-designator &rest params) in which
case the PARAMS are still treated as parameters (in order) but at run
time FUNCTION-DESIGNATOR is called on each of the actual arguments of
the PARAMS to transform said arguments before use by the underlying
query.  For example: (foo (\*to-json\* bar baz) blot) is an acceptable
**QUERY-PARAMS** list, as long as \*to-json\* is funcallable.

#### J->
*Macro*

```common-lisp
form1 &optional form2
```

S-SQL syntactic sugar to turn a single string **FORM1** into a
Postgres -> operation using the default JSON column 'jdoc and the
property FORM1; or to turn a symbol **FORM1** and string **FORM2** into a ->
operation using the specified JSON column **FORM1** and the property
**FORM2**.

#### J->>
*Macro*

```common-lisp
form1 &optional form2
```

S-SQL syntactic sugar to turn a single string **FORM1** into a Postgres
->> operation using the default JSON column 'jdoc and the property
FORM1; or to turn a symbol **FORM1** and string **FORM2** into a ->> operation
using the specified JSON column **FORM1** and the property **FORM2**.

#### TO-JSONB
*Macro*

```common-lisp
form
```

S-SQL syntactic sugar to cast **FORM** to the Postgres jsonb type.

#### JBUILD
*Macro*

```common-lisp
&rest key-forms
```

S-SQL syntactic sugar to create a new Postgres JSON object from the
**KEY-FORMS**, the structure of which are documented in the Postgres-JSON
User Guide.


---
## Model parameters
#### \*SEQUENCE\*
*DEFVAR*

```common-lisp
'pgj-seq
```

A symbol being the default name of the PostgreSQL sequence to
provide unique IDs for JSON objects inserted into the PostgreSQL
backend base table.

#### \*KEY\*
*DEFVAR*

```common-lisp
'key
```

A symbol being the name of the primary key column in backend
tables.

#### \*KEY-TYPE\*
*DEFVAR*

```common-lisp
'integer
```

A symbol being the type of the primary key column in backend
tables.

#### \*GIN-OPERATOR-CLASS\*
*DEFPARAMETER*

```common-lisp
'jsonb-ops
```

A symbol, which is TO-SQL-NAME converted to a string, being the
operator class of the GIN index created on the model relations.  See
Postgres 9.4 manual 8.14.4.

#### MAKE-MODEL-PARAMETERS
*Function*

```common-lisp
model &key (sequence *pgj-sequence*) (gin-operator-class *gin-operator-class*) (key *key*) (key-type *key-type*)
```

Create an object of class MODEL-PARAMETERS to specify backend
features of a PostgreSQL JSON persistence model **MODEL** (a symbol),
typically to be supplied to CREATE-MODEL.  For each keyword argument
which defaults to a special variable see the documentation of that
variable.


---
## Trival helper functions
#### STASH-KEY
*Function*

```common-lisp
key object
```

If **OBJECT** is a hash-table add the pair "key" => **KEY** to a copy of
the hash-table and return it.  Otherwise just return **OBJECT**.  You
might like to write you own verson which can handle objects besides
hash tables.

#### STASH-KEY-DESTRUCTIVE
*Function*

```common-lisp
key object
```

If **OBJECT** is a hash-table add the pair "key" => **KEY** to the
hash-table and return it.  Otherwise just return **OBJECT**.  You might
like to write you own verson which can handle objects besides hash
tables.

#### OBJ
*Function*

```common-lisp
&rest args
```

Return an 'equal key/value hash-table consisting of pairs of **ARGS**.
For JSON use your keys must be Common Lisp strings.

#### PP-JSON
*Function*

```common-lisp
object &key (stream *standard-output*) (indent 4)
```

Pretty print lisp **OBJECT** as JSON to **STREAM** with specified **INDENT**.


---
## Miscellaneous backend functions
#### CREATE-DB-SEQUENCE
*Function*

```common-lisp
sequence &optional (schema *pgj-schema*)
```

Create a PostgreSQL sequence with name **SEQUENCE** in **SCHEMA** (both symbols).
Requires an active DB connection.

#### ALTER-ROLE-SET-SEARCH-PATH
*Function*

```common-lisp
user &optional (search-path *default-search-path*)
```

Alter the role of Postgres user **USER**, a string, to set the
'search_path' setting to the string **SEARCH-PATH**.  In most cases this
is what you want so than when defining your own queries with
DEFINE-MODEL-QUERY unqualified relation names can be found in our
default schema (which is not the PUBLIC schema).  This setting does
_not_ effect the normal model interface functions such as GET and
FILTER as they use fully qualified relation names at all times.  Will
only take effect upon your next connection.  Beware, may be overridden
by settings in your ~/.psqlrc file.  See also the Postgres
documentation on search paths and settings.

#### FLUSH-PREPARED-QUERIES
*Function*

If you get a 'Database error 26000: prepared statement ... does not
exist error' while mucking around at the REPL, call this.  A similar
error in production code should be investigated.


---
## Specials
#### \*PGJ-SCHEMA\*
*DEFVAR*

```common-lisp
'pgj-model
```

A symbol being the name of the PostgreSQL schema we create to house
all database backend objects.  If you rebind it said binding must be
in place for _all_ calls to exported functions of :postgres-json.

#### \*DEFAULT-SEARCH-PATH\*
*DEFPARAMETER*

```common-lisp
(format nil "~A,public" (to-sql-name *pgj-schema*))
```

The default value used by ALTER-ROLE-SET-SEARCH-PATH.

#### \*TO-JSON\*
*DEFVAR*

```common-lisp
#'to-json
```

Function designator for function of one argument to serialize lisp
objects (submitted to INSERT and UPDATE, for example) to JSON.  Bind
it at run time for use by the model interface functions.  Or redefine
it globally for use in your own project.

#### \*FROM-JSON\*
*DEFVAR*

```common-lisp
#'yason:parse
```

Function designator for function of one argument to make lisp
objects from JSON strings retrieved from the DB backend.  Used by GET,
for example.  Bind it at run time for use by the model interface
functions.  Or redefine it globally for use in your own project.

#### \*STASH-KEY\*
*DEFVAR*

```common-lisp
#'stash-key-destructive
```

Function designator for function of two arguments: the value of the
unique primary key to be used for the DB insert and the object to be
inserted itself.  It should return an object which will be inserted in
the place of the original.

#### \*DB-HANDLE-SERIALIZATION-FAILURE-P\*
*DEFVAR*

```common-lisp
t
```

UPDATE and EXCISE calls on the model will use the Postgres
'repeatable read isolation level' so 'serialization failures' may
occur.  When this special variable is set to T (the default), these
failures are handled under the covers.  (However, if excessive time
elapses, client code may still see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE).  If you would rather
explicitly handle _all_ serialization failures in your client code,
set this to NIL.

#### \*SERIALIZATION-FAILURE-SLEEP-TIMES\*
*DEFVAR*

```common-lisp
'(0 1 2 4 7)
```

The length of this list of real numbers determines the number of
times to retry when a Postgres transaction COMMIT see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE condition.  For each retry we
sleep the duration specified, plus a random number of milliseconds
between 0 and 2000.  However, if 0 sleep is specified, we do not sleep
at all.


---

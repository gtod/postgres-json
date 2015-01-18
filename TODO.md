TODO
====

## Interface

* Timestamps in a JSON document: Either in the document or do we add
  extra columns to the model table?  First see just how far we can get
  with columns in the JSON doc...  But then they have to be just an
  integer...  Sure, but you can sort by them and local-time can
  reconstitute them...

## Want to have

* Allow raw importing of JSON, without the serialization step.

* unique ids from UUID example

* Make using Fkeys between tables easy...  "Promote" to Fkey as you
  can't do in JSON...  We could promote to foreign key: What key and
  Postgres type in your child model?  What master table?  We make a
  column, populate it, add the index For every insert, we grab that
  named key and stuff it into the FKey col Something similar for
  timestamps?

* Unicode handling tests.  UTF-8, 16?  For web deployment investigate
  quri.

* More extensive test suite.

* Revise, cull, doco once again.  Provide more motivating examples.

## Maybe have

* Define a schema for your model, get automatic validation either on
  client or server side.  Can do server side with PLV8 or whatever...

* Investigate new lateral type in Postgres.
  https://news.ycombinator.com/item?id=8689159 and
  http://www.postgresql.org/docs/9.4/static/queries-table-expressions.html#QUERIES-LATERAL

* Investigate making all integer keys bigints.  Seems like a premature
  optimization not too.  How hard would a manual migration be for the
  user?  Presumably no problem in lisp, but would have to alter table
  the various model tables, and sequences (indexes?  meta model?)

* How practical would it be to serialize FSET collections to JSON?
  Why would you do that?

## Nice to have

* Push changes to S-SQL and Postmodern (see postgres dir) upstream.

* Compound primary keys shouldn't be too hard. We make it either an
  ordered list or a map.  And stashing the key in your JSON obj would
  be the same.

* There are some fascinating Postgres functions for the jsonb type:
  `select distinct jsonb_object_keys(jdoc) from cat;`.  What use
  might we put them to?

## Postmodern/Postgres maybe have

### Server side JSON support

You could send a list of key/value pairs and do the JSON serialization
on the server: `json_object_agg`.  Server side update support.

PLV8.

### Prepared queries data types

Would be nice to cast `min-balance` to Postgres `jsonb` here (instead
of converting every balance to text and then to real) but Postmodern
prepared queries do not support types, so when we do `(to-json
min-balance)` the parser doesn't know what type min-balance is...

```common-lisp
(define-json-query rich-humans$ (min-balance gender)
  (:order-by
   (:select (jbuild ("key" "guid" "gender" "name" "balance"))
    :from 'human
    :where (:and (:>= (:type (j->> "balance") real) min-balance)
                 (:= (j->> "gender") gender)))
   (:type (j->> "balance") real)))
```

###  Batched Postgres row fetching

The postmodern layer of query and prepare over cl-postgres is pretty
straightforward if we need to dig down just one layer.

Looks like cl-postgres is *not* using named portals (but is using
named prepared statements).  How does this affect performance?  See:
http://www.postgresql.org/docs/9.4/static/protocol-overview.html#PROTOCOL-QUERY-CONCEPTS
specifically paragraph 3.  Can we/need we specialize
simple-execute-message to only **retrieve batches of rows at a time**?

Named portal optimized for multiple uses...  But it only lives inside the
transaction.  Would it make sense to have a 1-1 prepared stmt:named portal
map?  Ahh, a portal is a cursor, see
http://www.postgresql.org/docs/9.4/static/plpgsql-cursors.html
We could have a with-cursor(name) form and let the user
do what they like with the rows.  All this inside a single RO
tran?  http://www.pgcon.org/2014/schedule/attachments/330_postgres-for-the-wire.pdf

### Connection handling

Why did we see 50 open clients after running just 30 threads
(admittedly several times).  Was this a Lisp impl. thing?

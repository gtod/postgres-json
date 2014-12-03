TODO
====

## Must have

* Support querying of jsonb columns through Postmodern:

```sql
select c.jdoc->'name', c.jdoc->'coat', d.jdoc->'coat'
from cat c
join dog d on (c.jdoc->'name' = d.jdoc->'name');
```
* Investigate new lateral type in Postgres.
  https://news.ycombinator.com/item?id=8689159 and
  http://www.postgresql.org/docs/9.4/static/queries-table-expressions.html#QUERIES-LATERAL

* Investigate if should be using transaction time or actual time in
  model backend inserts.  Did get one primary key clash in some test,
  may have been related to dodgy transaction handling at the time...

* Does it make sense to do two updates to same row in one tran?

* Investigate making all integer keys bigints.  Seems like a premature
  optimization not too.  How hard would a manual migration be for the
  user?  Presumably no problem in lisp, but would have to alter table
  the various model tables, and sequences (indexes?  meta model?)

* Rethinkdb query to emulate

```common-lisp
(def-query-vector approved-orphaned-extra-charges (booking-ids) ()
  (re:r (:map (:eq-join (:map (:eq-join (:filter (:get-all (:table "extra_charges")
                                                           booking-ids
                                                           :index "bookingId")
                                                 (re:fn (extra-charge)
                                                   (:&& (:== "orphaned" (:row "state"))
                                                        (:row "approvedP"))))
                                        "typeId"
                                        (:table "extra_charge_types"))
                              (re:fn (row)
                                (:merge (:row "left")
                                        {o "type" (:attr (:row "right") "type")})))
                        "bookingId"
                        (:table "bookings"))
              (re:fn (row)
                {o "left" (:row "left")
                   "right" (:merge (:row "right")
                                   {o "orphanParent" t})}))))
```

* Test behaviour of nil, false, empty array etc on to-json, from-json
  etc.  Different libraries, different results.

### Interface

* Can the user user keywords instead of symbols when creating/accessing
  models?  Where else are symbols used, are they keyword safe?

* Filtering.  Simple filtering may not need full panoply of select
  support mentioned above.

* Get history of a specific object in a model.  With/without valid
  stamps...?  Document why this is useful --- clashing updates form two
  users are OK as either can see the full history and amend
  accordingly.

* Timestamps in a JSON document: Either in the document or do we add
  extra columns to the model table?  First see just how far we can get
  with columns in the JSON doc...  But then they have to be just an
  integer...  Sure, but you can sort by them and local-time can
  reconstitute them...

### Documentation

* Write a *User Guide* which is half tutorial and half reference.
  This way all the built in variation can be explained at greater
  leisure than in doc strings.

* We refer to "object" in the doc strings of the model interface
  functions.  Is this not a jdoc or JSON?  Depends on what point of
  serialization we are at.  Rethink.  Say what you mean.

## Want to have

* Allow raw importing of JSON, without the serialization step.

* unique ids from UUID example

* Stash valid_to, valid_from in objects from get-all?

* Would be nice to lisply abstract the JSON select stuff from PG.  Of
  course, you can still go to the DB directly if you like.
* Make using Fkeys between tables easy...

* Would be nice to get our public API and docstrings converted to
  Markdown or other github supported markup lang in the simplest
  possible way.

* Change the README to have the Quickstart right at the top.  Keep
  focused on ease of use, prove that.  Also change the example to show
  clearly the JSON strings and then the result of parsing them --- in
  the beginning I foundered a little on that point...

## Maybe have

* I could wrap the serialization failure condition and send a nice
  message to the user...  Maybe I wrap *all* Postmodern conditions and
  tell them the abstraction has leaked...?  Given the range of Postmodern
  conditions it may be possible to handle some sensible subset...

* How practical would it be to serialize FSET collections to JSON?
  Why would you do that?

## Nice to have

* Could write a macro to find any use of DB query functions (they end
  in $) and insert the appropriate ensure-model-query calls.  Little
  bit tricky for run time decisions about (say) sequence-nextval$ but
  that's a premature optimization anyway...

* Compound primary keys shouldn't be too hard. We make it either an
  ordered list or a map.  And stashing the key in your JSON obj would
  be the same.

* We've gone to a single backend schema for simplicity but it should not
  be too hard to support arbitrary schemata.  It's just I don't want to
  mess up the interface functions too much:

```
(insert 'cat (obj ...))

(insert '(animal cat) (obj ...))
```

## Postmodern/Postgres maybe have

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

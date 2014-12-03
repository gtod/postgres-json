TODO
====

Allow raw importing of JSON, without the serialization step.  In
fact people could just do this with PSQL and then we could get free
lisp objects from it!

I could wrap the serialization failure condition and send a nice
message to the user...  Maybe I wrap *all* Postmodern conditions and
tell them the abstraction has leaked...?  Given the range of Postmodern
conditions it may be possible to handle some sensible subset...

unique ids from UUID example

Stash valid_to, valid_from in objects from get-all?

* Support querying of jsonb columns through Postmodern:

```sql
select c.jdoc->'name', c.jdoc->'coat', d.jdoc->'coat'
from cat c
join dog d on (c.jdoc->'name' = d.jdoc->'name');
```

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

* Bulk inserts, clearly a need.

* Make using Fkeys between tables easy...

Test behaviour of nil, false, empty array etc on to-json, from-json
etc.  Different libraries, different results.

Would be nice to lisply abstract the JSON select stuff from PG.
Of course, you can still go to the DB directly if you like.

Compound primary keys shoudn't be too hard. We make it either an ordered
list or a map.

The model parameters could have a a single reader fn by string and
then we wouldn't need the entire fleet of specials!

How is our global query functions hash table cache affected by the use
of multiple threads?  And the model parameters?  And current
transaction settings?  Looks like we just need to add it to
bt:*default-special-bindings* ...  Or do we not need to?  Why can't
multiple threads share the same set of prepared queries?  Will
Postgres choke?

Test connection pooling.

Could write a macro to find any use of DB query functions (they end
in $) and insert the approrpiate ensure-model-query calls.  Little bit
tricky for run time decisions about (say) sequence-nextval$ but that's
a premature optimization anyway...

We've gone to a single backend schema for simplicity but it should not
be too hard to support arbitray schemas.  It's just I don't want to
mess up the interface functions too much:

```
(insert 'cat (obj ...))

(insert '(animal cat) (obj ...))
```

Would be nice to get our public API and docstrings converted to
Markdown or other github supported markup lang in the simplest
possible way.

Note so happy with wasting 10 mins updating non existant rows.
Can we have an option for :single! for the update$ ?

The postmodern layer of query and prepare over cl-postgres is pretty
straightforward if we need to dig down just one layer.  But focus
should probably move to the querying user interface

Change the README to have the Quickstart right at the top.  Keep
focused on ease of use, prove that.  Also change the example to show
clearly the JSON strings and then the result of parsing them --- in
the beginning I foundered a little on that point...

Looks like cl-postgres is *not* using named portals (but is using
named prepared statements).  How does this affect performance?  See:
http://www.postgresql.org/docs/9.4/static/protocol-overview.html#PROTOCOL-QUERY-CONCEPTS
specifically paragraph 3.  Can we/need we specialize
simple-execute-message to only retrieve batches of rows at a time?

Named portal optimized for multiple uses...  But it only lives inside the
transaction.  Would it make sense to have a 1-1 prepared stmt:named portal
map?  Ahh, a portal is a cursor, see
http://www.postgresql.org/docs/9.4/static/plpgsql-cursors.html
We could have a with-cursor(name) form and let the user
do what they like with the rows.  All this inside a single RO
tran?  http://www.pgcon.org/2014/schedule/attachments/330_postgres-for-the-wire.pdf

Change to using logical transactions.  Wrap top level interface
functions in transactions.

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

* stash-id is expecting a hash table - test and document

* Make using Fkeys between tables easy...

Test behaviour of nil, false, empty array etc on to-json, from-json
etc.  Different libraries, different results.

Would be nice to lisply abstract the JSON select stuff from PG.
Of course, you can still go to the DB directly if you like.

Compound primary keys shoudn't be too hard. We make it either an ordered
list or a map.  Also change name of ID to KEY to suit.

The model parameters could have a a single reader fn by string and
then we wouldn't need the entire fleet of specials!

How is our global query functions hash table cache affected by the use
of multiple threads?  Looks like we just need to add it to
bt:*default-special-bindings* ...

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

In fact, I don't think the users really need it but it would be nice
to make a schema just for running tests in, for example.  Can bind
*pgj-schema* to achieve this.

Would be nice to get our public API and docstrings converted to
Markdown in the simplest possible way.

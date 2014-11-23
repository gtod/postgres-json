TODO
====

Allow raw importing of JSON, without the serialization step.  In
fact people could just do this with PSQL and then we could get free
lisp objects from it!

I could wrap the serialization failure condition and send a nice
message to the user...  Maybe I wrap *all* Postmodern conditions and
tell them the abstraction has leaked...?  For now, no.

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

* stash-id is expecting a hash table - document

* Make using Fkeys between tables easy...

Test behaviour of nil, false, empty array etc on to-json, from-json
etc.  Different libraries, different results.

Would be nice to lisply abstract the JSON select stuff from PG.
Of course, you can still go to the DB directly if you like.




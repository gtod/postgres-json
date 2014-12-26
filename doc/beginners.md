Using JSON with Common Lisp for beginners
=========================================

JSON is a [lightweight data-interchange format](http://www.json.org)
which amounts to a text representation of *objects*, *arrays*,
*strings* and *numbers*.  **JSON is just text**: it's a text
blueprint for constructing the preceeding list of data
structures/types in any given programming language.
http://www.json.org is an excellent summary.

There are several JSON libraries for Common Lisp and they make
different choices about how to parse a given JSON text
into [Common Lisp objects](http://www.lispworks.com/documentation/lw50/CLHS/Body/26_glo_o.htm).

For example, a *JSON object* (**not** to be confused with the much
more general notion of Common Lisp *object* just mentioned) "is an
unordered set of name/value pairs", like this:

```json
{ "name": "Milo", "age": 21, "likes": "snow" }
```

In Common Lisp we could represent this many ways, a few of which
follow.  (We assume `stream` is an open stream to a file which
contains the above JSON):

```common-lisp
(yason:parse stream :object-as :alist)
(("name" . "Milo") ("age" . 21) ("likes" . "snow"))

(yason:parse stream :object-as :plist)
("name" "Milo" "age" 21 "likes" "snow")

(yason:parse stream :object-as :hash-table)
#<HASH-TABLE :TEST EQUAL :COUNT 3 {1004FA5763}>
```

where the last one is unreadble because a Common Lisp hash
table has no literal representation.  The point is that a *JSON
object* is just Unicode text in some file, and choices have to be
made about how to represent that as a Common Lisp *object*.

Something similar happens with the JSON *array*, and the special
values `true`, `false` and `null`:

```json
[ 28.8, 14, "Fred", true, false, null ]
```

```common-lisp
(yason:parse stream)
(28.8 14 "Fred" T NIL NIL)

(yason:parse stream :json-arrays-as-vectors t)
#(28.8 14 "Fred" T NIL NIL)

(yason:parse stream :json-booleans-as-symbols t :json-nulls-as-keyword t)
(28.8 14 "Fred" YASON:TRUE YASON:FALSE :NULL)
```

Postgres-JSON is agnostic about your choice of JSON library because it
is JSON strings that go into the Postgres backend, and JSON strings
that come out.

How your lisp objects are converted (or "serialized") into JSON
strings (often called *to-json*) and how those JSON strings are then
parsed or "deserialized" (*from-json*) back into Common Lisp objects
is the job of your fine JSON library.

It should now be clear that a more esoteric Common Lisp object such as
a symbol, a function or a CLOS object can't be directly serialized to
JSON.  It would first need to be shoehorned into an *object* or
*array* or *string* or *number* or `true` or `false` or `null` (that's
the entire list of possibilities, JSON is simple).

I tend to use Common Lisp hash tables for JSON objects and Common Lisp
vectors for JSON arrays.  The unreadability of the Common Lisp hash
table is easily fixed in the special case *where it only contains
strings, numbers, NIL and (nested as deep as you like) vectors, lists
and hash tables* because those Common Lisp objects have a JSON
representation (at least according to
[YASON](http://common-lisp.net/project/yason/)):

```common-lisp
(defun pp-json (object &key (stream *standard-output*) (indent 4))
  "Pretty print lisp OBJECT as JSON to STREAM with specified INDENT."
  (fresh-line stream)
  (let ((s (yason:make-json-output-stream stream :indent indent)))
    (yason:encode object s)))
```

and now

```common-lisp
(pp-json (yason:parse stream :object-as :hash-table))
{
    "name":"Milo",
    "likes":"snow",
    "age":21
}
```

When you run `pp-json` you are asking Yason to turn your Common Lisp
object (here a hash table) into **JSON text**.  It does this and sends
it to standard output.  The function in `*to-json*` defined as part of
Postgres-JSON does something similar (automatically) on your behalf
when you evaluate

```common-lisp
(insert 'cat (obj "name" "Milo" "likes" "snow" "age" 21))
```

You should now be asking what `obj` does.  It's a trivial function
that turns a list of pairs into a Common Lisp hash table.  Since the
JSON spec requires that the key (we call them *properties*) of a *JSON
object* must be a *JSON string* you must use Common Lisp strings as
the even numbered arguments to `obj`.

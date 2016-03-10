Edit JSON as s-expressions transparently
========================================

We use quasi-alists to represent objects:

    { "k1": v1, "k2": v2, ... } -> ((k1 v1) (k2 v2) ...)

We use Elisp vectors for JSON arrays, Elisp strings and numbers for
JSON strings and numbers, and the symbols `true`, `false`, and `null`
to represent the corresponding JSON values.

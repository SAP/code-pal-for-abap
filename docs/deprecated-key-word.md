[code pal for ABAP](../README.md) > [Deprecated Key Word Check](deprecated-key-word.md)

## Deprecated Key Word Check

### What is the Intent of the Check?

This check points out old syntax which should be replaced with newer notations instead.

### How does the check work?

This check searches for deprecated key words like: `MOVE` and `TRANSLATE` and suggests its replacement to news notations/functions.

```ABAP
" MOVE 'A' TO variable.
DATA(variable) = 'A'.

" TRANSLATE lowercase TO UPPER CASE.
DATA(uppercase) = to_upper( lowercase ).

```

REMARK: The check will be continuously enhanced with other deprecated ABAP Keywords.

### How to solve the issue?

Use the newer notations instead.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC DEPRECATED_KEY` which should placed right after the statement:

```abap
MOVE ….  "#EC DEPRECATED_KEY

TRANSLATE …. "#EC DEPRECATED_KEY
```

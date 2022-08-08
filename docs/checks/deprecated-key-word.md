[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Deprecated Key Word Check](deprecated-key-word.md)

## Deprecated Key Word Check

### What is the intent of the check?

This check searches for obsolete syntax elements which should be replaced with newer syntax elements instead.

### How does the check work?

This check searches for the statements starting with the following keywords:

* `MOVE`
* `TRANSLATE`

### How to solve the issue?

Please `MOVE` by a normal assignment statement using `=` and `TRANSLATE` by an equivalent functional expression using [string functions}(https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/abenprocess_functions.htm).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC DEPRECATED_KEY` which should placed right after the statement:

```abap
MOVE ….  "#EC DEPRECATED_KEY

TRANSLATE …. "#EC DEPRECATED_KEY
```

### Example


Before the check:

```abap
MOVE 'A' TO variable.
TRANSLATE lowercase TO UPPER CASE.
```

After the check:

```abap
DATA(variable) = 'A'.
DATA(uppercase) = to_upper( lowercase ).
```

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Omit Optional EXPORTING](omit-optional-exporting.md)

## Omit Optional EXPORTING

### What is the intent of the check?

In functional calls of methods that only have importing parameters and a returning parameter, the keyword `EXPORTING` in front of the parameter list at call sites is superfluous. This check searches for these superfluous instances of `EXPORTING`.

Anti-Pattern:
```abap
update(
  EXPORTING
    node   = /dirty/my_bo_c=>node-item
    key    = item->key
    data   = item
    fields = changed_fields ).
```

Pattern:
```abap
update( node   = /clean/my_bo_c=>node-item
        key    = item->key
        data   = item
        fields = changed_fields ).
```

### How does the check work?

This check searches for the usage of `EXPORTING` in functional method calls that have no `CHANGING`, `IMPORTING`, `RECEIVING` or `EXCEPTIONS` clauses.

### How to solve the issue?

Omit the optional keyword `EXPORTING`.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC OPTL_EXP` which should be placed right at the end of the statement:

```abap
  class->meth1( EXPORTING param1 = 'example' ). "#EC OPTL_EXP
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-the-optional-keyword-exporting)

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [RECEIVING Statement Usage Check](receiving-usage.md)

## RECEIVING Statement Usage Check

### What is the intent of the check?

This check searches for `RECEIVING` clauses in method calls which should no longer be used. The only case in which it is necessary to use `RECEIVING` rather than functional notation is when an `EXCEPTIONS` clause to catch classic exceptions is present.

### How to solve the issue?

Replace the `RECEIVING` clause with its functional equivalent:

```abap
DATA(sum) = aggregate_values( values ).
```
instead of
```abap
DATA sum TYPE i.
aggregate_values( EXPORTING values = values RECEIVING result = sum ).
```

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC RECEIVING_USAGE`:

```abap
aggregate_values(
    EXPORTING
        values = values
    RECEIVING
        result = DATA(sum) ).   "#EC RECEIVING_USAGE
```

### Further Readings & Knowledge

* [Clean ABAP - Omit RECEIVING](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-receiving)

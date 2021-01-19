[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [RECEIVING Statement Usage Check](receiving-usage.md)

## RECEIVING Statement Usage Check

### What is the Intent of the Check?

This check searches for the `RECEIVING` statement which should no longer be used.

### How to solve the issue?

`RECEIVING` shall not be used.

```abap
DATA(sum) = aggregate_values( values ).
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

* [ABAP Styleguides on Clean Code - Omit RECEIVING Statement](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#omit-receiving)

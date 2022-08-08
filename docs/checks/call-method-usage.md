[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [CALL METHOD Usage Check](call-method-usage.md)

## CALL METHOD Usage Check

### What is the intent of the check?

This check searches for `CALL METHOD` statements. Static occurrences, i.e. those where the called method is known statically at compile-time, of this statement are obsolete and should be replaced by their functional equivalents.

### How does the check work?

The check finds `CALL METHOD` statements that do not contain dynamic parts. Dynamic parts are indicated by the method being called being specified by a literal or character-like variable in parentheses or the usage of the additions `PARAMETER-TABLE` or `EXCEPTION-TABLE`.

### How to solve the issue?

Change the method calls using `CALL METHOD` to functional method calls.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CALL_METH_USAGE` which has to be placed after the `CALL METHOD` statement:

```abap
CALL METHOD method_name. "#EC CALL_METH_USAGE
```

### Example

Before the check:

```abap
DATA(class) = NEW object( ).
CALL METHOD class->method
    EXPORTING param = var.
```

After the check:

```abap
DATA(class) = NEW object( ).
class->method( var ).
```

### Further Readings & Knowledge

* [Clean ABAP: Avoid CALL METHOD Statement](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-calls)

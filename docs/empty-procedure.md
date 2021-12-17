[code pal for ABAP](../README.md) > [Empty Procedure Check](empty-procedure.md)

## Empty Procedure Check

### What is the Intent of the Check?

This check searches for empty methods, forms and modules.

### How does the check work?

The check highlights all empty code statements. Comments and Pragmas are also classified as "empty".

### How to solve the issue?

Remove the empty code blocks if they are not needed.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC EMPTY_PROCEDURE` which should be placed right after the `ENDMETHOD` statement:

```abap
METHOD method_name.

ENDMETHOD. "#EC EMPTY_PROCEDURE
```

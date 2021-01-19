[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Empty Catch](empty_catch.md)

## Empty Catch

### What is the Intent of the Check?

This check searches for empty `CATCH` blocks.

### How to solve the issue?

Fill the `CATCH` block with an exception handling.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC EMPTY_CATCH` which should to be placed after the opening statement of the empty `CATCH`:

```abap
TRY.
"some code
CATCH cx_error. "#EC EMPTY_CATCH
ENDTRY.

CATCH SYSTEM-EXCEPTIONS. "#EC EMPTY_CATCH
ENDCATCH.
```

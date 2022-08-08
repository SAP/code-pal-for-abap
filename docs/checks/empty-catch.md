[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Empty Catch](empty_catch.md)

## Empty Catch

### What is the intent of the check?

This check searches for empty `CATCH` blocks.

### How to solve the issue?

Perform meaningful exception handling in the `CATCH` block.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC EMPTY_CATCH` or `"#EC NO_HANDLER` which should to be placed after the opening statement of the empty `CATCH`.

Note that this check is the same as a check in the Extended Program Check (SLIN) delivered by SAP. That check accepts a pragma `##NO_HANDLER` for suppressing its findings that Code Pal cannot evaluate (pragmas are inaccessible to ordinary Code Inspector checks). We recommend that you *either* use this Code Pal check *or* the corresponding  SLIN check, but not both, since if you use both you get two findings for the exact same issue.

```abap
TRY.
"some code
CATCH cx_error. "#EC EMPTY_CATCH
ENDTRY.
```

```abap
CATCH SYSTEM-EXCEPTIONS. "#EC EMPTY_CATCH
ENDCATCH.
```

```abap
TRY.
"some code
CATCH cx_error. "#EC NO_HANDLER
ENDTRY.
```

```abap
CATCH SYSTEM-EXCEPTIONS. "#EC NO_HANDLER
ENDCATCH.
```

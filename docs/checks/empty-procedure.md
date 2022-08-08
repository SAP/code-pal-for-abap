[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Empty Procedure Check](empty-procedure.md)

## Empty Procedure Check

### What is the intent of the check?

This check searches for empty modularization units - methods, forms and function modules - since empty procedures usually fulfill no purpose and can be deleted.

### How does the check work?

A modularization unit counts as empty when it contains no ABAP statements. Comments and pragmas do not count as ABAP statements.

### How to solve the issue?

Delete the empty modularization unit unless its existence is functionally necessary. The only case where such empty procedures are needed is usually when implementing interface methods or redefining inheriting methods to explicitly do nothing.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC EMPTY_PROCEDURE` which should be placed right after the `ENDMETHOD` statement.

Note that this check is the same as a check in the Extended Program Check (SLIN) delivered by SAP. That check accepts a pragma `##NEEDED` for suppressing its findings that Code Pal cannot evaluate (pragmas are inaccessible to ordinary Code Inspector checks). We recommend that you *either* use this Code Pal check *or* the corresponding  SLIN check, but not both, since if you use both you get two findings for the exact same issue.


```abap
METHOD method_name.

ENDMETHOD. "#EC EMPTY_PROCEDURE
```

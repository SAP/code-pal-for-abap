[code pal for ABAP](../README.md) > [READ TABLE with Subsequent Memory Assignment Check](sub-assign-read-table.md)

## READ TABLE with Subsequent Memory Assignment Check

### What is the Intent of the Check?

This check aims to prevent undesired changes to internal tables using field symbols in context of the `READ TABLE` statement with `INTO` instead of `ASSIGNING`.

### How does the check work?

This check finds `READ TABLE` statements which use statement `INTO` for assignment but using a field symbol instead of a variable.

### How to solve the issue?

Use `ASSIGNING` instead of `INTO` in combination with field symbols as otherwise the selected table row will be overwritten.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC SUB_ASSIGN` which should be placed after the closing dot of the `READ TABLE` statement:

```abap
READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs>) WHERE key = '1'.
" Do something with <fs>

READ TABLE itab INTO <fs> WHERE key = '2'.    "#EC SUB_ASSIGN
" Do something with <fs>
```

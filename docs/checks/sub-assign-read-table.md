[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [READ TABLE with Subsequent Memory Assignment Check](sub-assign-read-table.md)

## READ TABLE with Subsequent Memory Assignment Check

### What is the Intent of the Check?

This check tries to prevent undesired changes to the content referenced by a field symbol. Using an `INTO` clause with a field symbol overwrites the referenced content when the programmer might have intended to assign the field symbol instead.

### How does the check work?

This check searches for `READ TABLE` statements which use an `INTO` clause with a field symbol as its target.

### How to solve the issue?

Use `ASSIGNING` instead of `INTO` in combination with field symbols.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC SUB_ASSIGN` which should be placed after the closing dot of the `READ TABLE` statement:

```abap
READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY key = '1'.
" Do something with <fs>

READ TABLE itab INTO <fs> WITH KEY key = '2'.    "#EC SUB_ASSIGN
" Do something with <fs>
```

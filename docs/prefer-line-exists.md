[code pal for ABAP](../README.md) > [Prefer LINE_EXISTS or LINE_INDEX to READ TABLE or LOOP AT](prefer-line-exists.md)

## Prefer LINE_EXISTS or LINE_INDEX to READ TABLE or LOOP AT

### What is the Intent of the Check?

Prefer `LINE_EXISTS` or `LINE_INDEX` over `READ TABLE` or `LOOP AT` as they avoid needlessly longer statements.

### How to solve the issue?

Preferably, use `LINE_EXISTS` to check whether the row of an internal table exists, and `LINE_INDEX` to check the row index.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC PREF_LINE_EX`:

```abap
  READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'. "#EC PREF_LINE_EX
```

```abap
  LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'. "#EC PREF_LINE_EX
    ...
  ENDLOOP.
```

### Example

Before the check:

```abap
  READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
  
  IF sy-subrc = 0.
    line_index = sy-tabix.
    line_exists = abap_true.
  ENDIF.
```

```abap
  LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
    line_index = sy-tabix.
    line_exists = abap_true.
    EXIT.
  ENDLOOP.
```

After the check:

```abap
  DATA(index) = line_index( my_table[ key = 'A' ] ).
  DATA(exists) = xsdbool( line_exists( my_table[ key = 'A' ] ) ).
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-line_exists-to-read-table-or-loop-at)

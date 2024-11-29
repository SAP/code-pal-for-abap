[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Prefer INSERT INTO TABLE to APPEND TO](prefer-insert-into-to-append.md)

## Prefer INSERT INTO TABLE to APPEND TO

### What is the intent of the check?

This check searches for `APPEND` statements and reports a finding. `INSERT INTO TABLE` works with all table and key types, thus making it easier for you to refactor the table's type and key definitions if your performance requirements change.

### How to solve the issue?

Use `INSERT INTO` instead of `APPEND TO`.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC PREF_INSERT_INT`:

```abap
   DATA prefer_insert_into_table TYPE TABLE OF string.
   APPEND `example` TO prefer_insert_into_table. "#EC PREF_INSERT_INT
```

### Example

Before the check:

```abap
   DATA prefer_insert_into_table TYPE TABLE OF string.
   APPEND `example` TO prefer_insert_into_table.
```

After the check:

```abap
   DATA prefer_insert_into_table TYPE TABLE OF string.
   INSERT `example` INTO TABLE prefer_insert_into_table.
```

### Further Readings & Knowledge

* [Clean ABAP - Prefer INSERT INTO TABLE to APPEND TO](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-insert-into-table-to-append-to)

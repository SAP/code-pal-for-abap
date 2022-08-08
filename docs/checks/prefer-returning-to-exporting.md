[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Prefer RETURNING to EXPORTING](prefer-returning-to-exporting.md)

## Prefer RETURNING to EXPORTING

### What is the intent of the check?

This check searches for methods in ABAP objects that have only a single exporting parameter. 

### How to solve the issue?

Change the exporting parameter to a returning parameter, if possible. This may not be possible when the exporting parameter is not fully typed (i.e. at least partly generic) as returning parameters must be fully typed.


### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC PREFER_RET` which has to be placed after the method declaration:

```abap
  METHODS get_name EXPORTING result TYPE string. "#EC PREFER_RET
```

### Example

Before the check:

```abap
  METHODS get_name EXPORTING result TYPE string.
```

After the check:

```abap
  METHODS get_name RETURNING VALUE(result) TYPE string.
```

### Further Readings & Knowledge

* [Clean ABAP: Prefer RETURNING to EXPORTING](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-returning-to-exporting)

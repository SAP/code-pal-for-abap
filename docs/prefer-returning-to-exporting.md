[code pal for ABAP](../README.md) > [Prefer RETURNING to EXPORTING](prefer-returning-to-exporting.md)

## Prefer RETURNING to EXPORTING

### What is the Intent of the Check?

Based on the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-returning-to-exporting), this check searches in classes and interfaces for methods that have only one exporting parameter. If it finds one, it will recommend you to change it from `EXPORTING` to `RETURNING`. 

### How to solve the issue?

Change the `EXPORTING` parameter to `RETURNING`.

:bulb: [A RETURNING parameter must be fully typed (#218)](https://github.com/SAP/styleguides/issues/218)

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

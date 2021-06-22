[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Prefer RETURNING to EXPORTING](prefer-returning-to-exporting.md)

## Prefer RETURNING to EXPORTING

### What is the Intent of the Check?


### How to solve the issue?

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC ` which has to be placed after the method declaration:

```abap

```

### Example

Before the check:

```abap

```

After the check:

```abap

```

### Further Readings & Knowledge

* [ABAP Styleguides on Clean Code: Prefer RETURNING to EXPORTING](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-returning-to-exporting)

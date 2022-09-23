[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Equals Sign Chaining](equals-sign-chaining.md)

## Equals Sign Chaining

### What is the intent of the check?

This check identifies chained assignments to multiple variables as they usually confuse the reader.

```abap
"anti-pattern
x = y = z
```

### How to solve the issue?

Break the assignment into multiple rows:

```abap
y = z.
x = y.
```

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo-comment `"#EC EQUALS_CHAINING` which should be placed after the attribution:

```abap
x = y = z.        "#EC EQUALS_CHAINING
```

### Further Readings & Knowledge

* [Clean ABAP - Don't chain assignments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-chain-assignments)

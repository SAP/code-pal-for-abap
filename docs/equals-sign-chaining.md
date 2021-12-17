[code pal for ABAP](../README.md) > [Equals Sign Chaining](equals-sign-chaining.md)

## Equals Sign Chaining

### What is the Intent of the Check?

This check identifies sequenced assignments as they usually confuse the reader.

```abap
"anti-pattern
x = y = z
```

### How to solve the issue?

Break it in multiple rows:
```abap
y = z.
x = y.
```

Alternatively, you can use the `xsdbool` if the target is a comparison:

```abap
x = xsdbool( y = z ).
```

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo-comment `"#EC EQUALS_CHAINING` which should be placed after the attribution:

```abap
x = y = z.        "#EC EQUALS_CHAINING
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-xsdbool-to-set-boolean-variables)

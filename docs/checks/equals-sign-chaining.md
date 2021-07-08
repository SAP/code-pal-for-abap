[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Equals Sign Chaining](equals-sign-chaining.md)

## Equals Sign Chaining

### What is the Intent of the Check?

This check searches for chained boolean assignments in the code which might generate some confusion with an equal condition.

### How does the check work?

The check searches for chained assignments of boolean variables.

```abap
DATA x TYPE abap_bool.
DATA y TYPE abap_bool.
DATA z TYPE abap_bool.

x = abap_false.
y = abap_false.
z = abap_true.

x = y = z.
```

### How to solve the issue?

Use `xsdbool( conditions )` to allocate the result of conditions into a variable.

```abap
DATA x TYPE abap_bool.
DATA y TYPE abap_bool.
DATA z TYPE abap_bool.

x = abap_false.
y = abap_false.
z = abap_true.

x = xsdbool( y = abap_false AND
             z = abap_true ).
```

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo-comment `"#EC EQUALS_CHAINING` which should be placed after the attribution:

```abap
DATA x TYPE abap_bool.
DATA y TYPE abap_bool.
DATA z TYPE abap_bool

x = abap_false.
y = abap_false.
z = abap_true.

x = y = z.        "#EC EQUALS_CHAINING
```

### Further Readings & Knowledge

* [SAP Code Style Guides](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-xsdbool-to-set-boolean-variables)

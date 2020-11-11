# code pal for ABAP

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [CHECK Statement Position Check](check-statement-position.md)

## CHECK Statement Position Check

### What is the Intent of the Check?

The “Check Statement Position” Check is part of the Clean Code Check Repository.

This check proves that the CHECK statement is the first statement in a method, function module or form routine.

### Which attributes can be maintained?

![Attributes](./imgs/check_statement_position.png)

### How to solve the issue?

The CHECK statement shall be the first statement or use an IF statement instead.

### What to do in case of exception?

In special cases you can suppress this Check's finding by using the pseudo comment `"#EC CHECK_POSITION`.

### Example

```abap
METHOD method_name.

1 some ABAP source code.

2 CHECK condition. "#EC CHECK_POSITION

3 some more ABAP source code.

ENDMETHOD.
```

# code pal for ABAP

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [CHECK Statement Position Check](check-statement-position.md)

## CHECK Statement Position Check

### What is the Intent of the Check?

The “Check Statement Position” verifies whether the CHECK statement is in the first position (first statement) within a method, function-module or form-routine.
Do not use CHECK outside of the initialization section of a method. The statement behaves differently in different positions and may lead to unclear, unexpected effects.

### Which attributes can be maintained?

![Attributes](./imgs/check_statement_position.png)

### How to solve the issue?

The CHECK statement shall be the first statement of a method (suggested even before any DATA declaration); if not, try to substitute this keyword by an IF-statement instead.

### What to do in case of exception?

In special cases you can suppress this finding by using the pseudo comment `"#EC CHECK_POSITION`.

### Example

```abap
METHOD method_name.

1 some ABAP source code.

2 CHECK condition. "#EC CHECK_POSITION

3 some more ABAP source code.

ENDMETHOD.
```

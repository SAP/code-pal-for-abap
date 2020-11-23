# code pal for ABAP

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [CHECK in LOOP](check-in-loop.md)

## CHECK in LOOP

### What is the Intent of the Check?
It verifies whether the `CHECK` statement is found inside of a `LOOP` statement. A CHECK within a LOOP, ends the current iteration and proceeds to the next one. This behaviour might lead to confusion: Does it end the method processing or does it exit the loop?

### How to solve the issue?
Prefer using `CONTINUE` within an IF-Statement instead (since the keyword `CONTINUE` can only be used in loops, the intention is clear to everyone reading the code).
Keep also in mind, the other Keywords like `EXIT` and `RETURN` are also more explicit.

### What to do in case of exception?
In special cases you can suppress this finding by using the pseudo comment `"#EC CHECK_IN_LOOP`.

```abap
LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
  CHECK <tadir>-delflag = abap_true. "#EC CHECK_IN_LOOP
ENDLOOP.
```

### Example
Before the check:
```abap
LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
  CHECK <tadir>-delflag = abap_true.
ENDLOOP.
```

After the check:
```abap
LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
  IF <tadir>-delflag = abap_false.
    CONTINUE.
  ENDIF.
ENDLOOP.
```

### Further Readings & Knowledge
- [Avoid CHECK in other positions (Clean ABAP)](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#avoid-check-in-other-positions)
- [Exiting Loops -> Check](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm)

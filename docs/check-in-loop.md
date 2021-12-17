[code pal for ABAP](../README.md) > [CHECK in LOOP](check-in-loop.md)

## CHECK in LOOP

### What is the Intent of the Check?
This check verifies if a `CHECK` statement is being used inside of a `LOOP` structure. A CHECK within a LOOP, ends the current iteration and proceeds to the next one. This behavior might lead to some confusion like: Does it end the method processing or does it exit the loop?

### How to solve the issue?
Prefer using `CONTINUE` (within an IF-Statement) instead of using the CHECK Statement in this case. Since the keyword `CONTINUE` can only be used in LOOPS, the intention is then automatic clear to everyone reading the code.
Keep also in mind, that other Keywords like: `EXIT` or `RETURN` are also more explicit than `CHECK`.

### What to do in case of exception?
In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CHECK_IN_LOOP` which should be placed after the `CHECK` Statement itself:

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
- [Clean ABAP: Avoid CHECK in other positions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-check-in-other-positions)
- [Exiting Loops -> Check](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm)

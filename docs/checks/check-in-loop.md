[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [CHECK in LOOP](check-in-loop.md)

## Check in Loop

### What is the intent of the check?
This check searches for `CHECK` statements inside iterations (such as `LOOP...ENDLOOP` or `DO...ENDDO`). A `CHECK` within a loop ends the current instance of the iteration and proceeds to the next one. Since `CHECK` statements that are not inside a loop leave the current modularization unit, e.g. inside a method act like a conditional `RETURN` statement, it can be confusing to determine the effect of any given `CHECK` statement.

### How to solve the issue?
Replace the `CHECK` statement by its equivalent `IF` statement
```abap
IF not condition.
  CONTINUE.
ENDIF.
```
and take care to choose a form of the logical expression that is easy to interpret. Since the keyword `CONTINUE` can only be used in loops, the intent of continuing to the next instance of the iteration is now unambiguous.

### What to do in case of exception?
In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CHECK_IN_LOOP` which should be placed after the `CHECK` statement itself:

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
  "some more code...
ENDLOOP.
```

After the check:
```abap
LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
  IF <tadir>-delflag = abap_false.
    CONTINUE.
  ENDIF.
  "some more code...
ENDLOOP.
```

Note that in this example, the cleanest way of expressing the intent behind the code would be to remove the statement inside the loop completely and replace it by a `WHERE` clause in the `LOOP` statement:

```abap
LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>) WHERE delflag = abap_false.
  "some more code...
ENDLOOP.
```

### Further Readings & Knowledge
- [Clean ABAP: Avoid CHECK in other positions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-check-in-other-positions)
- [Exiting Loops -> Check](https://help.sap.com/doc/abapdocu_756_index_htm/7.56/en-US/abapcheck_loop.htm)

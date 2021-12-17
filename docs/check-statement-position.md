[code pal for ABAP](../README.md) > [CHECK Statement Position Check](check-statement-position.md)

## CHECK Statement Position Check

### What is the Intent of the Check?
This check verifies whether the `CHECK` statement is the very first statement within a method, function-module or form-routine.  

The [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-check-in-other-positions) says:
> Do not use `CHECK` outside of the initialization section of a method. The statement behaves differently in different positions and may lead to unclear, unexpected effects.

REMARKS: 
1. CHECK statement inside of LOOPs will be disregard by check (for that, refer to: CHECK_IN_LOOP). 
2. The usage of CLEAR statement prior to CHECK statement is considered to be a bad coding practice! This is actually a workaround in bad designed code (against OO scope principles). 
3. DATA declarations (DATA / FIELD-SYMBOLS when not dynamic declared – inline declarations), might also come before the keyword CHECK.

### How to solve the issue?
The `CHECK` statement shall be the first statement of a method. If it is not possible, try to substitute this keyword with an IF-statement instead.

### What to do in case of exception?
In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CHECK_POSITION` which has to be placed after the `CHECK` statement:

```abap
METHOD example.
  some code...
  CHECK condition = abap_true. "#EC CHECK_POSITION
```

### Example
Before the check:
```abap
METHOD example.
  ...
  CHECK sy-mandt = 000.
  ...
ENDMETHOD.
```

After the check:
```abap
METHOD example.
  ...
  IF sy-mandt <> 000.
    RETURN.
  ENDIF.
  ...
ENDMETHOD.
```
OR

```abap
METHOD example.
  CHECK sy-mandt = 000.
  ...
ENDMETHOD.
``` 

### Further Readings & Knowledge
- [Clean ABAP: Avoid CHECK in other positions (Clean ABAP)](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-check-in-other-positions)

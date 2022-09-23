[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [CHECK Statement Position Check](check-statement-position.md)

## CHECK Statement Position Check

### What is the intent of the check?
This check searches for `CHECK` statements that are not the first statement within a method, function module or form subroutine since the statement behaves differently in different positions and may lead to unclear, unexpected effects.

### How does the check work

When the check finds a `CHECK` statement, it will report a finding unless one of the following circumstances applies:

1. The statement occurs within a loop - see [Check in Loop](check-in-loop.md) for a check dealing with these statements.
2. The only statement before the `CHECK` statement are variable declarations. However, see the check for [chained declarations](chain-declaration-usage.md) for better practices when declaring variables.

While it might sometimes seem necessary to have a `CLEAR` statement for exporting parameters in front of any `CHECK` statements, a need for both of these statements indicates a method that should be refactored to be less confusing, so the check still reports a finding for these cases.

### How to solve the issue?
Either move the `CHECK` statement to be the first statement of the method or replace it with its equivalent `IF` statement.

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
or

```abap
METHOD example.
  ...
  IF sy-mandt = 000.
    ...
  ENDIF.
ENDMETHOD.
```
or

```abap
METHOD example.
  CHECK sy-mandt = 000.
  ...
ENDMETHOD.
``` 

Note how the second option expresses most clearly the intent both syntactically and visually - the rest of the method (the part indented inside the `IF` statement) is to be executed if the condition is true.
### Further Readings & Knowledge
- [Clean ABAP: Avoid CHECK in other positions (Clean ABAP)](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-check-in-other-positions)

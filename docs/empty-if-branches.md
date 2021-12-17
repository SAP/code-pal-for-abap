[code pal for ABAP](../README.md) > [Empty IF Branches Check](empty-if-branches.md)

## Empty IF-Branch Check

### What is the Intent of the Check?

This check searches for empty `IF` statements or branches.

### How to solve the issue?

Fill the empty `IF` structure with code or remove it by refactoring the condition.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC EMPTY_IF_BRANCH` which should be placed after the opening statement of the empty `IF` branch; or, in case of nested `IF`statements, in the deepest structure within the branch:

```abap
IF name = ''.
  IF address = ''. "#EC EMPTY_IF_BRANCH

  ENDIF.
ENDIF.

IF name = ''. "#EC EMPTY_IF_BRANCH

ENDIF.

IF name = ''.
  " Source Code.
ELSEIF name = ''. "#EC EMPTY_IF_BRANCH

ELSE.
  " Source Code.
ENDIF.

IF name = ''.
  " Source Code.
ELSE. "#EC EMPTY_IF_BRANCH

ENDIF.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#no-empty-if-branches)

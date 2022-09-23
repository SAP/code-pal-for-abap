[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Empty IF Branches Check](empty-if-branches.md)

## Empty IF-Branch Check

### What is the intent of the check?

This check searches for empty branches of `IF` statements.

### How to solve the issue?

Fill the empty `IF` structure with code or remove it by refactoring the condition.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC EMPTY_IF_BRANCH` which should be placed after the opening statement of the empty `IF` branch; or, in case of nested `IF`statements, in the deepest structure within the branch.

Note that this check is the same as a check in the Extended Program Check (SLIN) delivered by SAP. That check accepts a pragma for suppressing its findings that Code Pal cannot evaluate (pragmas are inaccessible to ordinary Code Inspector checks). We recommend that you *either* use this Code Pal check *or* the corresponding  SLIN check, but not both, since if you use both you get two findings for the exact same issue.


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

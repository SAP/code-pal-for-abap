[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [FORM Routine Check](form-routine.md)

## FORM Routine Usage Check

### What is the intent of the check?

This check searches for the usage of form subroutines since this concept became obsolete with the release of object-oriented ABAP.

### How does the check work?

This check searches for `ENDFORM.` statements.

### How to solve the issue?

Use classes and methods instead as these are the intended tool for modularization in object-oriented ABAP.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_FORM` which should be placed right after the `FORM` declaration:

```abap
FORM my_form. "#EC CI_FORM
  " Form content
ENDFORM.
```

### Further Readings & Knowledge

* [Clean ABAP - Prefer object orientation to procedural programming](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-object-orientation-to-procedural-programming)

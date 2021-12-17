[code pal for ABAP](../README.md) > [FORM Routine Check](form-routine.md)

## FORM Routine Usage Check

### What is the Intent of the Check?

This check searches for the usage of FORM Routines (procedural programming) since with the release of Object Oriented ABAP this syntax became obsolete.

### How does the check work?

This check searches for the usage of the `ENDFORM` statement.

### How to solve the issue?

Use classes and methods instead. Methods are similar to subroutines and can be used for modularization.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_FORM` which should be placed right after the `FORM` declaration:

```abap
FORM my_form. "#EC CI_FORM
  " Form content
ENDFORM.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-object-orientation-to-procedural-programming)

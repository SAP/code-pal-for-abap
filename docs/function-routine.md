[code pal for ABAP](../README.md) > [FUNCTION Routine Check](function-routine.md)

## FUNCTION Routine Usage Check

### What is the Intent of the Check?

This check searches for the usage of FUNCTION Modules (procedural programming) since with the release of Object-Oriented ABAP this syntax became obsolete.

### How does the check work?

This check searches for Function Modules within a function group. However, since Remote Function Calls (RFC) can only be executed via FUNCTION MODULES, these ones (Function Modules with RFC enabled) will not be caught by this check.

Note: This check does not search for the `CALL FUNCTION` statement within your source code (e.g. in a method or program).

### How to solve the issue?

Use classes and methods instead (Object Oriented ABAP).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_FUNCTION` which should be placed after the FUNCTION statement:

```abap
FUNCTION my_function. "#EC CI_FUNCTION
    " Function content
ENDFUNCTION.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-object-orientation-to-procedural-programming)

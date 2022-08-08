[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [FUNCTION Routine Check](function-routine.md)

## FUNCTION Routine Usage Check

### What is the intent of the check?

This check searches for the usage of function modules since non-RFC-enabled function modules became obsolete with the release of object-oriented ABAP.

### How does the check work?

When a function group is checked, this check will determine the RFC status of all function modules inside it and report a finding for every function module that is not RFC-enabled.

### How to solve the issue?

Use classes and methods instead as these are the intended tool for modularization in object-oriented ABAP.

### What to do in case of exception?

There are no pseudo comments for this check since pseudo comments cannot be used in the definition part of a function module.

### Further Readings & Knowledge

* [Clean ABAP - Prefer object orientation to procedural programming](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-object-orientation-to-procedural-programming)

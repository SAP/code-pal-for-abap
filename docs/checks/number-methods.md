[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Number of Methods Check](number-methods.md)

## Number of Methods Check

### What is the intent of the check?

This check counts the number of methods defined by an ABAP object and reports a finding when this exceeds a configurable threshold. If there are too many methods in a class, this can be an indicator that the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) is violated.

### How does the check work?

This check counts `METHODS` and `CLASS-METHODS` statements within a global or local class definition or interface definition. Inherited methods are mostly not counted, but redefined inherited methods are.

### How to solve the issue?

Split the class or interface into multiple classes or interfaces, each containing a subset of the current methods where the methods in subset relate to the same responsibility.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUMBER_METHODS` which should be placed right after the class definition header.

Note that this check is equivalent to a subset of the "OO Size Metrics" check delivered by SAP. That check accepts no pseudo comments or pragmas. We recommend that you *either* use this Code Pal check *or* the SAP-delivered check, but not both, since if you use both you get two findings for the exact same issue.

```abap
CLASS class_name DEFINITION.   "#EC NUMBER_METHODS
  METHODS method_name_one.
  CLASS-METHODS method_name_two.
ENDCLASS.
```

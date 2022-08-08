[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Number of Attributes Check](number-attributes.md)

## Number of Attributes Check

### What is the intent of the check?

This check counts the number of attributes of an ABAP object and reports a finding when this exceeds a configurable threshold. When a class has many attributes, this can be an indicator that the [single responsibility principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) (SRP) is violated.

### How does the check work?

This check counts `DATA` and `CLASS-DATA` declarations within a global or local class definition or interface definition. Inherited attributes and constants are not counted as attributes for the purpose of this check. A structure is counted as one attribute, no matter how many components are in the structure.

### How to solve the issue?

If the SRP is violated, split the class or interface into multiple classes or interfaces, each containing a subset of its current attributes. Otherwise, if all attributes are needed for a single responsibility, consider grouping the attributes together in a meaningful structure or hierarchy of structures.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUMBER_ATTR` which should be placed right after the class definition header.

Note that this check is equivalent to a subset of the "OO Size Metrics" check delivered by SAP. That check accepts no pseudo comments or pragmas. We recommend that you *either* use this Code Pal check *or* the SAP-delivered check, but not both, since if you use both you get two findings for the exact same issue.

```abap
CLASS class_name DEFINITION.   "#EC NUMBER_ATTR
  DATA data_1 TYPE t1.
  DATA data_2 TYPE t1.
  CLASS-DATA class_data_1 TYPE t1.
ENDCLASS.
```

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Number of Interfaces Check](number-interfaces.md)

## Number of Interfaces Check

### What is the intent of the check?

This check counts the number of interfaces implemented by an ABAP object and reports a finding when this exceeds a configurable threshold. If there are too many interfaces in a class, this can be an indicator that the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) is violated.

### How does the check work?

This check counts `INTERFACES` statements within a within a global or local class definition or interface definition. Inherited interfaces are not counted.

### How to solve the issue?

If the interfaces belong to semantically distinct responsibilities, split the class or interface into multiple classes or interfaces, each with a single of the current interfaces. If multiple interfaces actually relate to the same kind of functionality, consider consolidating these interfaces into a single interface.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NMBR_INTERFACES` which should be placed right after the class definition header.

Note that this check is equivalent to a subset of the "OO Size Metrics" check delivered by SAP. That check accepts no pseudo comments or pragmas. We recommend that you *either* use this Code Pal check *or* the SAP-delivered check, but not both, since if you use both you get two findings for the exact same issue.

```abap
CLASS class_name DEFINITION.   "#EC NMBR_INTERFACES
  INTERFACES interface_name_one.
  INTERFACES interface_name_two.
ENDCLASS.
```

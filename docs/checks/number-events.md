[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Number of Events Check](number-events.md)

## Number of Events Check

### What is the intent of the check?

This check counts the number of events of an ABAP object and reports a finding when this exceeds a configurable threshold. When a class has too many events, this can be an indicator that the [single responsibility principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) (SRP) is violated.

### How does the check work?

The check counts `EVENTS` and `CLASS-EVENTS` statements within a global or local class definition or interface definition. Inherited events are not counted.

### How to solve the issue?

Split the class or interface into multiple classes or interfaces, each containing a subset of its current events.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUMBER_EVENTS` which should be placed right after the class definition header.

Note that this check is equivalent to a subset of the "OO Size Metrics" check delivered by SAP. That check accepts no pseudo comments or pragmas. We recommend that you *either* use this Code Pal check *or* the SAP-delivered check, but not both, since if you use both you get two findings for the exact same issue.

```abap
CLASS class_name DEFINITION.   "#EC NUMBER_EVENTS
  EVENTS event_name_one.
  CLASS-EVENTS event_name_two.
ENDCLASS.
```

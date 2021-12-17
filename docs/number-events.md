[code pal for ABAP](../README.md) > [Number of Events Check](number-events.md)

## Number of Events Check

### What is the Intent of the Check?

This check counts the number of events up to a maximum. When a class has too many events, it is an indicator that the [single responsibility principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) is violated.

### How does the check work?

The check counts `EVENTS` and `CLASS-EVENTS` within a global or local, `CLASS DEFINITION` or `INTERFACE`.

### How to solve the issue?

Split the class or interface into multiple classes or interfaces which then contain less events.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUMBER_EVENTS` which should be placed right after the class definition header:

```abap
CLASS class_name DEFINITION.   "#EC NUMBER_EVENTS
  EVENTS event_name_one.
  CLASS-EVENTS event_name_two.
ENDCLASS.
```

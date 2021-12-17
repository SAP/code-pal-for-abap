[code pal for ABAP](../README.md) > [Number of Methods Check](number-methods.md)

## Number of Methods Check

### What is the Intent of the Check?

This check counts the number of methods up to a maximum. If there are too many methods in a class, it is an indicator that the [single responsibility principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) is violated.

### How does the check work?

This check counts `METHODS` and `CLASS-METHODS` within a global or local, `CLASS DEFINITION` or `INTERFACE`. Inherited methods are not counted, however `REDEFINED METHODS` increment the counter.

### How to solve the issue?

The solution is to split the class or interface into multiple classes or interfaces which then contain less methods.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUMBER_METHODS` which should be placed right after the class definition header:

```abap
CLASS class_name DEFINITION.   "#EC NUMBER_METHODS
  METHODS method_name_one.
  CLASS-METHODS method_name_two.
ENDCLASS.
```

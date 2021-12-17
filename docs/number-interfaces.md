[code pal for ABAP](../README.md) > [Number of Interfaces Check](number-interfaces.md)

## Number of Interfaces Check

### What is the Intent of the Check?

This check counts the number of interfaces up to a maximum. If there are too many interfaces in a class, it is an indicator that the [single responsibility principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) is violated.

### How does the check work?

This check counts `INTERFACES` within a global or local, `CLASS DEFINITION` or `INTERFACE`.

### How to solve the issue?

Split the class or interface into multiple classes or interfaces which then contain less interfaces.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NMBR_INTERFACES` which should be placed right after the class definition header:

```abap
CLASS class_name DEFINITION.   "#EC NMBR_INTERFACES
  INTERFACES interface_name_one.
  INTERFACES interface_name_two.
ENDCLASS.
```

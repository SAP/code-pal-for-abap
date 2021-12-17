[code pal for ABAP](../README.md) > [Number of Attributes Check](number-attributes.md)

## Number of Attributes Check

### What is the Intent of the Check?

This check counts the number of attributes up to a maximum. When a class has has too many attributes, this is probably an indicator that the [single responsibility principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) is violated.

### How does the check work?

This check counts only `DATA` and `CLASS-DATA` within a global or local, `CLASS DEFINITION` or `INTERFACE`. Inherited attributes and all constants are not counted. A structure is counted as one attribute, no matter how many attributes are in the structure.

### How to solve the issue?

Split the class or interface into multiple classes or interfaces which then contain less attributes. If there are many attributes related to one task, it's possible to group the attributes in structures.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUMBER_ATTR` which should be placed right after the class definition header:

```abap
CLASS class_name DEFINITION.   "#EC NUMBER_ATTR
  DATA data_1 TYPE t1.
  DATA data_2 TYPE t1.
  CLASS-DATA class_data_1 TYPE t1.
ENDCLASS.
```

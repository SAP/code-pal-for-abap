[code pal for ABAP](../README.md) > [Number of Public Attributes Check](number-public-attributes.md)

## Number of Public Attributes Check

### What is the Intent of the Check?

This check counts the number of public attributes. All attributes should be private (by default) or protected (if needed). The [data encapsulation principle](https://en.wikipedia.org/wiki/Encapsulation_(computer_programming)) helps you to protect your attributes from being changed and adds readability for others; the basic principle: ["One only sees what is needed"](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#members-private-by-default-protected-only-if-needed).

### How does the check work?

This Check counts only `DATA` and `CLASS-DATA` within a global or local, `CLASS DEFINITION` or `INTERFACE`. Inherited attributes and `CONSTANTS` aren't counted. A structure is counted as one attribute, no matter how many attributes are in the structure.

### How to solve the issue?

Make those attributes `PRIVATE` or `PROTECTED`. You can grant the read access with a getter method (for example, `get_user_first_name`). With a setter, you can also grant write access and have the possibility to validate the inserted data (for example, `set_time_in_seconds` with a test to allow only positive numbers).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUM_PUBLIC_ATTR` which should be placed right after the `PUBLIC SECTION` statement:

```abap
CLASS class_name DEFINITION.  
  PUBLIC SECTION. "#EC NUM_PUBLIC_ATTR
    DATA attribute1 TYPE i.
    DATA attribute2 TYPE i.
ENDCLASS.
```

### Further Reading
* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#members-private-by-default-protected-only-if-needed) 



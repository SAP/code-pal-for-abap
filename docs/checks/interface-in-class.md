[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Interface in Class Check](interface-in-class.md)

## Interface Missing Check

### What is the intent of the check?

This check searches for classes with public methods that are not part of an interface. Methods that are not part of an interface make decoupling more difficult and, especially if the class is declared as final, are much harder to mock in unit tests of consumers.

### How does the check work?

This check searches for declarations of methods within the public section of a class that are not redefinitions. It does not care about static methods (`CLASS-METHODS`).

### How to solve the issue?

Make sure to create an interface for public methods.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC INTF_IN_CLASS` which should be placed right after the `PUBLIC SECTION` statement:

```abap
CLASS class_name DEFINITION.
  PUBLIC SECTION. "#EC INTF_IN_CLASS
ENDCLASS.
```

### Further Readings & Knowledge

* [Clean ABAP - Public instance methods should be part of an interface](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#public-instance-methods-should-be-part-of-an-interface)

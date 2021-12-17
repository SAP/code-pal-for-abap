[code pal for ABAP](../README.md) > [Interface in Class Check](interface-in-class.md)

## Interface Missing Check

### What is the Intent of the Check?

This check searches for classes having public methods without an interface.

### How does the check work?

Since every class having at least one public method should implement an interface, this check searches for public methods within a class without having an associated interface (being implemented). 

### How to solve the issue?

Make sure to implement an interface for the public methods. Even though this seems to be unnecessary in some cases, having an interface will easily allow mocking data in the future.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC INTF_IN_CLASS` which should be placed right after the `PUBLIC SECTION` statement:

```abap
CLASS class_name DEFINITION.
  PUBLIC SECTION. "#EC INTF_IN_CLASS
ENDCLASS.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#public-instance-methods-should-be-part-of-an-interface)

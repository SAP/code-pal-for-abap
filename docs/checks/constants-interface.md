[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Constants Interface Check](constants-interface.md)

## Constants Interface Check

### What is the intent of the check?

This check is meant to prevent the creation of interfaces whose sole purpose is to define constants. You should usually prefer [enumeration classes](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-enumeration-classes-to-constants-interfaces) to interfaces that only declare constants.

### How does the check work?

The check searches for interfaces containing only constants.
### How to solve the issue?

Use enumeration classes instead.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CONS_INTF` which should be placed right after the class definition header:

```abap
INTERFACE interface_name.   "#EC CONS_INTF
    CONSTANTS two TYPE i VALUE 2.
ENDINTERFACE.
```

### Example

Before the check:

```abap
INTERFACE /dirty/message_severity.
  CONSTANTS:
    error        TYPE symsgty VALUE 'E',
    warning      TYPE symsgty VALUE 'W', 
    notification TYPE symsgty VALUE 'N'.
ENDINTERFACE.
```

After the check:

```abap
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
       error        TYPE symsgty VALUE 'E',
       warning      TYPE symsgty VALUE 'W', 
       notification TYPE symsgty VALUE 'N'.
ENDCLASS.
```
or
```abap
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      error        TYPE REF TO /clean/message_severity READ-ONLY,
      warning      TYPE REF TO /clean/message_severity READ-ONLY,
      notification TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

### Further Readings & Knowledge

* [Clean ABAP - Prefer enumeration classes to interfaces with constants](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-enumeration-classes-to-constants-interfaces)

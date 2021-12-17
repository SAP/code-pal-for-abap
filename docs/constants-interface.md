[code pal for ABAP](../README.md) > [Constants Interface Check](constants-interface.md)

## Constants Interface Check

### What is the Intent of the Check?

This check intends to avoid the creation and usage of an interface object for merely defining constants.
You should always prefer [enumeration classes](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-enumeration-classes-to-constants-interfaces) to constants interfaces.

```abap
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
       error        TYPE symsgty VALUE 'E',
       warning      TYPE symsgty VALUE 'W', 
       notification TYPE symsgty VALUE 'N'.
ENDCLASS.
```
or even
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

### How does the check work?

The "Constant Interface" check searches for interfaces containing only constants.

### How to solve the issue?

Use enumeration classes instead.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CONS_INTF` which should be placed right after the class definition header:

### Example

```abap
INTERFACE interface_name.   "#EC CONS_INTF
    CONSTANTS two TYPE i VALUE 2.
ENDINTERFACE.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-enumeration-classes-to-constants-interfaces)

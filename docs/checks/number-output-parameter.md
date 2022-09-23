[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Number of Output Parameters Check](number-output-parameter.md)

## Number of Output Parameters Check

### What is the intent of the check?

This check searches for methods where more than one output parameter is used. Methods should [do one thing and do it well](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#do-one-thing-do-it-well-do-it-only), and having multiple output parameters indicates that that is not the case.

### How does the check work?

The check reports a finding whenever a method has more than one output parameter. An "output" parameter is any exporting, changing or returning parameter.

### How to solve the issue?

If the method has only a single responsibility and the output parameters belong together logically, express that by making them all part of the same structure and using that structure as the output parameter. If the method has multiple responsibilities, follow the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) and create a dedicated method for each responsibility.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC NUM_OUTPUT_PARA` which should be placed right after the method definition:

```abap
CLASS class_name DEFINITION.
  PUBLIC SECTION.
    METHOD method_name
      EXPORTING
        param1 TYPE c
        param2 TYPE i
      CHANGING
        param2  TYPE c
      RETURNING
        VALUE(result) TYPE c.  "#EC NUM_OUTPUT_PARA
ENDCLASS.
```

### Further Reading

 - [Clean ABAP - Parameter Number](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#parameter-number)
 - [Clean ABAP - Parameter Types](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#parameter-types)
[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Nesting Depth Check](maximum-nesting-depth.md)

## Nesting Depth Check

### What is the intent of the check?

This check computes the maximal nesting depth of structures within a modularization unit. A high nesting depth is an indicator that the source code might be difficult to read and understand, making maintaining and extending the code also more difficult.

### How does the check work?

The nesting depth of structures at a point is defined by the number of currently open structures (like `LOOP...ENDLOOP`, `IF...ENDIF`) at that point. In well-formatted code this usually corresponds to the level of indentation. 

The check reports a finding when the maximum over all nesting depths inside a modularization unit exceeds the configured threshold.

### How to solve the issue?

Modularize the functionality into smaller blocks. This increases the readability and efficiency.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_NESTING` which should be placed right after the `ENDMETHOD` statement:

```abap
METHOD method_name.
...
  IF any_condition1.
    IF any_condition2.
      IF any_condition3.
        IF any_condition4.
          IF any_condition5.
            any_variable = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD. "#EC CI_NESTING
```

### Further Readings & Knowledge

* [Clean ABAP - Keep the Nesting Depth Low](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#keep-the-nesting-depth-low)

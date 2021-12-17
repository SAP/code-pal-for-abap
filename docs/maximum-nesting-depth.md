[code pal for ABAP](../README.md) > [Nesting Depth Check](maximum-nesting-depth.md)

## Nesting Depth Check

### What is the Intent of the Check?

This check counts the nesting-depth level of a method, function-module, form-routine or module. A high value of nesting depth is an indicator that the source code might be difficult to read (no readability). Furthermore, maintaining and extending the code is also more difficult. In addition, the risk of introducing bugs is increased with a high nesting depth value.

### How does the check work?

The check verifies if the number of nested code blocks within a method, function module, form routine or module has reached/exceeded a defined threshold (configurable).

REMARK: The `TEST-SEAM` statement does not count to the level of nesting depth (it is ignored).

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

* [Clean ABAP - keep the Nesting Depth low](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#keep-the-nesting-depth-low)

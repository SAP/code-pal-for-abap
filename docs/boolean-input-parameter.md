[code pal for ABAP](../README.md) > [Boolean Input Parameter](boolean-input-parameter.md)

## Boolean Input Parameter

### What is the Intent of the Check?

This check searches for the usage of boolean input parameters in a method signature. These parameters, could be an indicator of bad design where the Single Responsibility Principle is not followed (the method might be doing several things instead of a single thing).   

REMARK: Setter methods using boolean input variables are acceptable.

### How to solve the issue?

Splitting the method may simplify its code and provide a better description for the consumer.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC BOOL_PARAM` which has to be placed after the method declaration:

```abap
  METHODS update IMPORTING do_save TYPE abap_bool. "#EC BOOL_PARAM
```

### Example

Before the check:

```abap
  METHODS update IMPORTING do_save TYPE abap_bool.
```

After the check:

```abap
  METHODS update_without_saving.
  METHODS update_and_commit.
```

### Further Readings & Knowledge

* [Clean ABAP: Boolean Input Parameter(s)](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#split-method-instead-of-boolean-input-parameter)

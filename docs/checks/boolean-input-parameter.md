[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Boolean Input Parameter](boolean-input-parameter.md)

## Boolean Input Parameter

### What is the intent of the check?

This check searches for boolean input parameters in method signatures. These parameters could be an indicator that the [single-responsibility principle (SRP)](https://en.wikipedia.org/wiki/Single-responsibility_principle) is not followed since the method might be doing several things at once.   

Setter methods using boolean input variables are acceptable when the variable being set is a boolean.

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

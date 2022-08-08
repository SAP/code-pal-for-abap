[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Optional Parameters](optional-parameters.md)

## Optional Parameters

### What is the intent of the check?

This check searches for optional parameters in method signatures. It is recommended to avoid optional parameters because they might confuse the consumers of the class about questions like:

* Which parameters are really required?
* Which combination of parameters are valid?
* Which parameters exclude each other?

It also becomes harder to perform any changes in the method since all possible combinations of its parameters must always be considered.

Multiple methods with specific parameters for their respective use cases avoid this confusion by giving clear guidance which parameter combinations are expected.

### How to solve the issue?

Create new methods dedicated to their specific use case instead of adding optional parameters to a generic method.

### Example

Before the check:

```abap
  METHODS do_one_or_the_other
    IMPORTING
      what_i_need    TYPE string OPTIONAL
      something_else TYPE i OPTIONAL.
```

After the check:

```abap
  METHODS do_one_thing IMPORTING what_i_need TYPE string.
  METHODS do_another_thing IMPORTING something_else TYPE i.
```

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC OPTL_PARAM` which should to be placed after the method declaration:

```abap
  METHODS do_one_or_the_other
    IMPORTING
      what_i_need    TYPE string OPTIONAL
      something_else TYPE i OPTIONAL. "#EC OPTL_PARAM
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#split-methods-instead-of-adding-optional-parameters)

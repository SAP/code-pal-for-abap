[code pal for ABAP](../README.md) > [Optional Parameters](optional-parameters.md)

## Optional Parameters

### What is the Intent of the Check?

This check searches for OPTIONAL parameter in method signatures. It is recommended to avoid the usage of `OPTIONAL` parameters because they might confuse the consumers of the class:

* Which parameters are really required?
* Which combination of parameters are valid?
* Which parameters exclude each other?

Multiple methods with specific parameters for the use case avoid this confusion by giving clear guidance which parameter combinations are valid and expected.

### How to solve the issue?

Splitting methods (creating new ones) instead of adding `OPTIONAL` parameters.

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

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Unit-Test Assert Validator](unit_test_assert.md)

## Unit-Test Assert Validator

### What is the Intent of the Check?

This check finds invalid assertions in unit tests. It recognizes methods following the patterns `CL_ABAP_UNIT_ASSERT=>ASSERT*` and `CL_AUNIT_ASSERT=>ASSERT*` as unit test assertions.

### How does the check work?

It checks for invalid values for the actual (`act`) or expected (`exp`) values in an assertion. An invalid value is one that leads to an assertion that is always true or always false, for instance:
- when both are using the same variable for the assertion (always true)
- when both are hardcoded (always true or always false)

### How to solve the issue?

Change the actual (`act`) or expected (`exp`) value(s) to represent a meaningful assertion.

### What to do in case of exception?

In exceptional cases (if any), you can suppress this finding by using the pseudo comment `"#EC UT_ASSERT` which has to be placed after the assertion statement:

```abap
cl_abap_unit_assert=>assert_equals( act = sum 
                                    exp = sum ). "#EC UT_ASSERT 
```

### Example

Before the check:

```abap
METHOD sum. 
  " given 
  DATA(first) = 10. 
  DATA(second) = 10. 
  " when 
  DATA(sum) = first + second. 
  " then 
  cl_abap_unit_assert=>assert_equals( act = sum 
                                      exp = sum ).
ENDMETHOD. 
```

After the check:

```abap
METHOD sum. 
  " given 
  DATA(first) = 10. 
  DATA(second) = 10. 
  " when 
  DATA(sum) = first + second. 
  " then 
  cl_abap_unit_assert=>assert_equals( act = sum 
                                      exp = 20 ).
ENDMETHOD. 
```

### Further Readings & Knowledge

* [Clean ABAP: Don't obsess about coverage](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-obsess-about-coverage)
* [Unit testing with ABAP unit](https://help.sap.com/docs/SAP_S4HANA_CLOUD/25cf71e63940453397a32dc2b7676947/08c60b52cb85444ea3069779274b43db.html?q=abap%20unit%20test)





[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Unit-Test Assert Validator](unit_test_assert.md)

## Unit-Test Assert Validator

### What is the intent of the check?

This check searches for assertions in unit tests that do not fulfill any actual purpose. 

### How does the check work?

The check looks at the parameters `act` and `exp` of calls to methods whose name contains `assert`. It raises a finding if both parameters are identical or if both are literals.

### How to solve the issue?

Change the actual (`act`) or expected (`exp`) value(s) to represent a meaningful assertion.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC UT_ASSERT` which has to be placed after the assertion statement:

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





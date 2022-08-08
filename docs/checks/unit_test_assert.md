[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Unit-Test Assert Validator](unit_test_assert.md)

## Unit-Test Assert Validator

### What is the intent of the check?

This check searches for assertions in unit tests that do not fulfill any actual purpose. 

### How does the check work?

The check looks at the parameters `act` and `exp` of calls to methods whose name contains `assert`. It raises a finding if both parameters are identical or if both are literals.

### How to solve the issue?

Fix the actual (`act`) or expected (`exp`) value(s) in the unit test assertion in order to achieve a meaningful and real Assertion.

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
* [ABAP Test Tools & Frameworks](https://pages.github.tools.sap/EngineeringCulture/ase/ABAP/abapTestTools.html)





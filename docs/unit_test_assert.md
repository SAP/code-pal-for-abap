[code pal for ABAP](../README.md) > [Unit-Test Assert Validator](unit_test_assert.md)

## Unit-Test Assert Validator

### What is the Intent of the Check?

This check verifies invalid assertions in unit tests.  
It supports the `CL_ABAP_UNIT_ASSERT=>ASSERT*` and `CL_AUNIT_ASSERT=>ASSERT*`.

### How does the check work?

It checks for actual (`act`) or expected (`exp`) invalid value(s), for instance:
- When both are using the same variable for the Assertion (which will always return TRUE);
- When both are hardcoded.

### How to solve the issue?

Fix the actual (`act`) or expected (`exp`) value(s) in the unit test assertion in order to achieve a meaningful and real Assertion.

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
* [ABAP Test Tools & Frameworks](https://pages.github.tools.sap/EngineeringCulture/ase/ABAP/abapTestTools.html)





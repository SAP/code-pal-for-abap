[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Unit-Test Missing Assert](unit_test_missing_assert.md)

## Unit-Test Missing Assert

### What is the intent of the check?

This check identifies unit test methods that do not include any assertions while utilizing the `CL_ABAP_UNIT_ASSERT` class. A unit test that lacks any assertions or validation of expected behavior is fundamentally meaningless.

### How does the check work?

The check evaluates each unit test method to determine whether at least one occurrence of the `CL_ABAP_UNIT_ASSERT` class is present. A finding is raised if no static methods of `CL_ABAP_UNIT_ASSERT` have been implemented.

### How to solve the issue?

Please include a meaningful assertion using `ASSERT...`, or invoke `SKIP` or `FAIL` to automate a specific behavior of the test method.

### What to do in case of exception?

Currently, there is no pseudo comment available to disable the check.

### Example

Before the check:

```abap
METHODS initialize FOR TESTING.

...

  METHOD initialize.

    f_cut = NEW #( ).

  ENDMETHOD.
```

After the check:

```abap
METHODS initialize FOR TESTING.

...

  METHOD initialize.
    f_cut = NEW #( ).

    cl_abap_unit_assert=>assert_bound(
      act   = f_cut
      msg   = 'Instance was not created. Why?' ).
  ENDMETHOD.
```

### Further Readings & Knowledge

* [Clean ABAP: Don't obsess about coverage](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-obsess-about-coverage)
* [Unit testing with ABAP unit](https://help.sap.com/docs/SAP_S4HANA_CLOUD/25cf71e63940453397a32dc2b7676947/08c60b52cb85444ea3069779274b43db.html?q=abap%20unit%20test)





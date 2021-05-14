[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [`cut` as Default](cut-as-default.md)

## `cut` as Default

### What is the Intent of the Check?

In a test class, the code under the test can be represented using a meaningful name, or `cut` as a default.  
If you decide to follow `cut` as a default, this Check reports test methods that do not follow the pattern.

### How does the check work?

For each `for testing` method, it searches for the `cut` on it, and also on its `class definition`.

### How to solve the issue?

Name the code under the test to `cut`.

### What to do in case of exception?

If you found a meaningful name, you can suppress this finding by using the pseudo comment `"#EC CUT_AS_DEFAULT` which has to be placed after the method implementation statement:

```abap
METHOD test. "#EC CUT_AS_DEFAULT 
  " given
  DATA demo_failures TYPE REF TO y_demo_failures. 
  " when
  demo_failures = NEW #( ). 
  " then 
  cl_abap_unit_assert=>assert_bound( demo_failures ). 
ENDMETHOD. 
```

### Example

Before the check:


```abap
METHOD test.
  DATA class_abc TYPE REF TO ...
  DATA class_123 TYPE REF TO ...
  DATA class_qwe TYPE REF TO ...
  ...
  cl_abap_unit_assert=>assert_bound( class_qwe ). 
ENDMETHOD. 
```

After the check:

```abap
METHOD test.
  DATA class_abc TYPE REF TO ...
  DATA class_123 TYPE REF TO ...
  DATA cut TYPE REF TO ...
  ...
  cl_abap_unit_assert=>assert_bound( cut ). 
ENDMETHOD. 
```

### Further Readings & Knowledge

* [Clean ABAP: Name the code under test meaningfully, or default to CUT](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#name-the-code-under-test-meaningfully-or-default-to-cut)



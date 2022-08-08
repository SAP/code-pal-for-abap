[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Deprecated Classes](deprecated-classes.md)

## Deprecated Classes

### What is the intent of the check?

This check searches for the usage of deprecated objects which should be replaced by newer objects.

### How does the check work?

The check searches for occurrences of the following objects:

* `CL_AUNIT_ASSERT`
* `IF_AUNIT_CONSTANTS`

### How to solve the issue?

Reference non-deprecated/newer objects instead. For the above example, a corrected code would look like:

```abap
DATA aunit TYPE REF TO cl_abap_unit_assert.
```

### What to do in case of exception?

In exceptional cases (e.g.: for keeping release compatibility), you can suppress this finding by using the pseudo comment `"#EC DEPRECATED_CLAS` which should be placed right after the statement:

```abap
DATA aunit TYPE REF TO cl_aunit_assert. "#EC DEPRECATED_CLAS` 
```


[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Method Return Bool Check](method-return-bool.md)

## Method Name Misleading for Boolean Return Check

### What is the intent of the check?

This check searches for methods that return a boolean value whose name does not indicate that they return a boolean. In particular in the case of predicative method calls it is important that such methods are named in a way that expresses the boolean nature of their return value clearly.

### How does the check work?

The check searches for method declarations whose returning parameter has the type `ABAP_BOOL` and reports a finding if the method does not either start with any of the following strings:

* `is_`
* `has_`
* `are_`
* `try_`
* `can_`
* `have_`
* `starts_`
* `ends_`
* `must_`
* `should_`
* `was_`
* `were_`

or contains one of the following words:

* `exist`
* `equal`
* `contain`

### How to solve the issue?

Rename the method to properly reflect the boolean nature of its return value.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC METH_RET_BOOL`:

```abap
METHODS calc
    IMPORTING
        number TYPE i
    RETURNING
        VALUE(result) TYPE abap_bool.     "#EC METH_RET_BOOL
```

[code pal for ABAP](../README.md) > [Method Return Bool Check](method-return-bool.md)

## Method Name Misleading for Boolean Return Check

### What is the Intent of the Check?

This check aims for a meaningful name for methods returning boolean valued types.

### How does the check work?

The check searches for methods with a boolean returning value and then verifies if the name starts with one of the following words:

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

or if the name contains one of the following words:

* `exist`
* `equal`
* `contain`

### How to solve the issue?

Rename the method and start with is, has, are, have or contains.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC METH_RET_BOOL`:

```abap
METHODS calc
    IMPORTING
        number TYPE i
    RETURNING
        VALUE(result) TYPE abap_bool.     "#EC METH_RET_BOOL
```

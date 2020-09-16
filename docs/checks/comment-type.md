# Code Pal for ABAP

[Code Pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Comment Type](comment-type.md)

## Comment Type

### What is the Intent of the Check?

Comments with `"`, not with `*`, because asterisked comments tend to indent to weird places.

### How to solve the issue?

Replacing the comment type from `*` to `"`.

### Example

Before the check: 

```abap
  METHOD do_it.
    IF input IS NOT INITIAL.
* delegate pattern
      output = calculate_result( input ).
    ENDIF.
  ENDMETHOD.
```

After the check:

```abap
  METHOD do_it.
    IF input IS NOT INITIAL.
      " delegate pattern
      output = calculate_result( input ).
    ENDIF.
  ENDMETHOD.
```

### Further Readings & Knowledge

* [ABAP Styleguides on Clean Code](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#comment-with--not-with-)

# code pal for ABAP

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Comment Type](comment-type.md)

## Comment Type

### What is the Intent of the Check?

This check searches for comments in the code marked up with `"` instead of with `*`. Comments marked up with an asterisk tend to be indented to weird/uncontrolled places.

### How to solve the issue?

You should replace the comment sign from `*` to `"`.

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

* [ABAP Styleguides on Clean Code: Comment sign](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#comment-with--not-with-)

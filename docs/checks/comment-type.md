[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Comment Type](comment-type.md)

## Comment Type

### What is the intent of the check?

This check searches for comments starting with an asterisk `*`. These tend to not align well with the rest of the code, especially when the surrounding code changes indentation level, and should be replaced by comments starting with a quotation mark `"`.

### How to solve the issue?

You should replace the `*` by `"`.

### What to do in case of exception?
There are no pseudo comments for this check since you cannot put pseudo comments on the same line as an actual comment.

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

* [Clean ABAP - Comment with ", not with *](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#comment-with--not-with-)

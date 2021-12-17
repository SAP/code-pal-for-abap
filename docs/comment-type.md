[code pal for ABAP](../README.md) > [Comment Type](comment-type.md)

## Comment Type

### What is the Intent of the Check?

This check searches for comments in the code marked up with `"` instead of with `*`. Comments marked up with an asterisk tend to be indented to weird/uncontrolled places.

### How to solve the issue?

You should replace the comment sign from `*` to `"`.

### What to do in case of exception?
There is no exception for this check since it works as an indicator only. Thus, it is also not possible to suppress its findings.

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

* [Clean ABAP: Comment sign](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#comment-with--not-with-)

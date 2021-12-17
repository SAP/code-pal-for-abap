[code pal for ABAP](../README.md) > [Comment Position](comment-position.md)

## Comment Position

### What is the Intent of the Check?

This check searches for "Quote comments" which are not indented along with the statements they belong to.

### How to solve the issue?

You should indent the comments along with the statements they are commenting.

### What to do in case of exception?
There is no exception for this check since it works as an indicator only. Thus, it is also not possible to suppress its findings.

### Example

Before the check:

```abap
" delegate pattern
  output = calculate_result( input ).
```

```abap
  output = calculate_result( input ). " delegate pattern
```

After the check:

```abap
  " delegate pattern
  output = calculate_result( input ).
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#put-comments-before-the-statement-they-relate-to)

# Code Pal for ABAP

[Code Pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Comment Position](comment-position.md)

## Comment Position

### What is the Intent of the Check?

Quote comments indent along with the statements they comment.

### How to solve the issue?

Identing the comments along with the statements.

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

* [ABAP Styleguides on Clean Code](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#put-comments-before-the-statement-they-relate-to)

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Comment Position](comment-position.md)

## Comment Position

### What is the intent of the check?

This check finds comments starting with `"` that are not indented at the same level as the statements that follow them. Comments should generally refer to the code that follows them, and hence should be indented at the same level.

The check does not report findings for comments that are in-line comments inside of a multi-line statement or that are the only "code" inside an empty branch of a conditional statement. In-line comments are reported when they occur at the end of a statement.

### How to solve the issue?

You should indent comments along with the statements they are commenting.

### What to do in case of exception?
There are no pseudo comments for this check since you cannot put pseudo comments on the same line as an actual comment.

### Example

Before the check:

```abap
" comment 1
  output = calculate_result( input ).
  var_1 = var_2. " comment 2
  select * from tab
    " comment 3
    where field_1 = var_3.
```

After the check:

```abap
  " comment 1
  output = calculate_result( input ).
  " comment 2  
  var_1 = var_2. 
  select * from tab
    " comment 3
    where field_1 = var_3.
```
or


### Further Readings & Knowledge

* [Clean ABAP - Put comments before the statement they relate to](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#put-comments-before-the-statement-they-relate-to)

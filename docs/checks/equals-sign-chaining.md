[BACK](../check_documentation.md)

# Equals Sign Chaining
## What is the Intent of the Check?
The Equals Sign Chaining Check searches for multiple declarations to a variable.

## How does the check work?
The check highlights when someone confuses the declaration of a variable with a condition.
### Example
```abap
DATA x TYPE bool.
DATA y TYPE bool.
DATA z TYPE bool.
x = y = z.
```

## How to solve the issue?
Use xsdbool( condition ) to allocate a Boolean into a variable.

### Example
```abap
DATA x TYPE bool.
DATA y TYPE bool.
DATA z TYPE bool.
x = xsdbool( y = abap_true AND
             z = abap_false ).
```

## Can the check be surpressed?
No.

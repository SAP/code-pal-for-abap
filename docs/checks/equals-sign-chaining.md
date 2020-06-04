[BACK](../check_documentation.md)

# Equals Sign Chaining
## What is the Intent of the Check?
The Equals Sign Chaining Check searches for multiple declarations (chained) to a variable.

## How does the check work?
The check highlights when someone confuses the declaration of a variable with a condition.
Moreover, when the sequence of the attribution remains unclear.
### Example
```abap
DATA x TYPE bool.
DATA y TYPE bool.
DATA z TYPE bool.
x = abap_false.
y = abap_false.
z = abap_true.
x = y = z.

Output:
x: true - changed
y: true - changed
z: true - unchanged
```

## How to solve the issue?
Use xsdbool( condition ) to allocate the result of a condition (or multiple conditions) into a variable.

### Example
```abap
DATA x TYPE bool.
DATA y TYPE bool.
DATA z TYPE bool.
x = abap_false.
y = abap_false.
z = abap_true.
x = xsdbool( y = abap_false AND
             z = abap_true ).

Output:
x: true  - changed because of xsdbool()
y: false - unchanged
z: true  - unchanged
```

## Can the check be surpressed?
No.

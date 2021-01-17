[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Scope of Variable](scope-of-variable.md)

## Scope of Variable

### What is the Intent of the Check?
If a variable is declared in a statement, it should be used/referred to inside this statement only (not outside).

### How does the check work?
It searches for `DATA` and `FIELD-SYMBOLS` declaration inside of `IF`, `ELSEIF`, `ELSE`, `DO`, `CASE/WHEN`, `LOOP`, and `WHILE` statements, and for its usage/reference outside this statement.

### How to solve the issue?
Relocate the declaration.

### What to do in case of exception?
In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC SCOPE_OF_VAR` which should be placed right after the variable usage/referece:

```abap
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2. "#EC SCOPE_OF_VAR
ENDIF.
```

### Example

Before:
```abap
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```

After:
```abap
DATA value TYPE i.

IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

### Further Readings & Knowledge
* [ABAP Styleguides on Clean Code](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#dont-declare-inline-in-optional-branches)

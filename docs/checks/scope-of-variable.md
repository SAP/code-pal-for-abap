[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Scope of Variable](scope-of-variable.md)

## Scope of Variable

### What is the intent of the check?
If a variable is declared inside a code block that forms a meaningful subdivision of scope (such as the branches of an `IF` statement), it should be used only inside that subdivision. ABAP lacks this sort of subdivided scope handling on a language level and does not prevent such confusing accesses - the smallest scope in ABAP is the method (excluding helper variables in certain constructor expressions) and any variable declared in a method remains visible until the end of the method.

### How does the check work?
The check searches for variable declarations (`DATA`, `FIELD-SYMBOLS`) inside of `IF`, `ELSEIF`, `ELSE`, `DO`, `CASE/WHEN`, `LOOP`, and `WHILE` blocks and for the usage of these variables outside these blocks.

### How to solve the issue?
Relocate the declaration if the access to the variable is intended or use a different variable for the outside access.

### What to do in case of exception?
In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC SCOPE_OF_VAR` which should be placed right after the variable usage/reference:

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
DATA(value) = 0.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

### Further Readings & Knowledge
* [Clean ABAP - Don't declare inline in optional branches](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-declare-inline-in-optional-branches)

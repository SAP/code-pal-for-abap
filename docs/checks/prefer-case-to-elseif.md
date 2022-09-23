[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Prefer CASE to ELSE IF](prefer-case-to-elseif.md)

## Prefer CASE to ELSEIF

### What is the intent of the check?

Conditions with many branches, i.e. `IF` statements with many `ELSEIF` alternatives, are often hard to read. When applicable, a `CASE` statement where all branches are on equal footing as `WHEN` branches is often much more readable. 

The check reports a finding when the number of branches exceeds a configurable threshold.

### How does the check work?

The check finds `IF`-`ELSEIF` chains where each condition consists only of a single logical expression and the first token of all logical expressions is the same. 

### How to solve the issue?

Use `CASE` instead of `ELSEIF` for multiple alternative conditions.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC PREFER_CASE`.

```abap
IF type = type-some_type. "#EC PREFER_CASE
  " ...
ELSEIF type = type-some_other_type.
  " ...
ELSE.
  RAISE EXCEPTION NEW /dirty/unknown_type_failure( ).
ENDIF.
```

### Example

Before the check:

```abap
IF type = types-some_type.
  " ...
ELSEIF type = types-some_other_type.
  " ...
ELSE.
  RAISE EXCEPTION NEW /dirty/unknown_type_failure( ).
ENDIF.
```

After the check:

```abap
CASE type.
  WHEN type-some_type.
    " ...
  WHEN type-some_other_type.
    " ...
  WHEN OTHERS.
    RAISE EXCEPTION NEW /clean/unknown_type_failure( ).
ENDCASE.
```

### Further Readings & Knowledge

* [Clean ABAP - Prefer CASE to ELSEIF](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-case-to-else-if-for-multiple-alternative-conditions)

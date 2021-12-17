[code pal for ABAP](../README.md) > [Prefer CASE to ELSE IF](prefer-case-to-elseif.md)

## Prefer CASE to ELSE IF

### What is the Intent of the Check?

Prefer `CASE` to `ELSEIF` for multiple alternative conditions because `CASE` makes it easy to see a set of alternatives that exclude each other. It can be faster than a series of `IF`s because it can translate to a different microprocessor command instead of a series of subsequently evaluated conditions. You can introduce new cases quickly, without having to repeat the discerning variable over and over again. The statement even prevents some errors that can occur when accidentally nesting the `IF`-`ELSEIF`s.

In short: If the IF-Statement has operations (like AND, OR, …) over several variables/attributes, it is not worthwhile to revert it to a CASE-Statement. In other words, conditions on multiple variables can be left in IF-statements. Only condition(s) on single variables should be converted to a CASE-Statement. 

The threshold determines the maximum number of conditions.

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
IF type = type-some_type.
  " ...
ELSEIF type = type-some_other_type.
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

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-case-to-else-if-for-multiple-alternative-conditions)

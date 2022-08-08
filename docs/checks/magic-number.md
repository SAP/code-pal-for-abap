[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Magic Number Check](magic-number.md)

## Magic Number Usage Check

### What is the intent of the check?

This check searches for numeric literals used directly without a meaningful constant declaration, often called magic numbers. Magic numbers hide their meaning from the user (e.g. whether they are a default value or have a specific meaning in the current context) and make refactoring unnecessarily hard when the same literal occurs in several places.

### How does the check work?

The check searches for numeric literals in the following statements:

1. `IF`
2. `ELSEIF`
3. `WHEN`
4. `CHECK`
5. `DO`

A finding is reported for every such literal that is not `0`, `1` or a power of ten unless it is used as an index in a table expression or an operand in a comparison with the result of a `LINES` function.

### How to solve the issue?

Replace all numeric literals by constants with a meaningful name. 

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_MAGIC` which should be placed right after the statement containing the magic number.

Note that there is a very similar check in the Extended Program Check (SLIN) delivered by SAP, although it differs in its scope as it is not restricted to specific statement types. That check accepts a pragma `##NUMBER_OK` for suppressing its findings that Code Pal cannot evaluate (pragmas are inaccessible to ordinary Code Inspector checks). We recommend that you *either* use this Code Pal check *or* the corresponding  SLIN check, but not both, since if you use both you get two findings for the exact same issue.

```abap
DO 5 TIMES. "#EC CI_MAGIC
  " Loop content
ENDDO.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-constants-instead-of-magic-numbers)

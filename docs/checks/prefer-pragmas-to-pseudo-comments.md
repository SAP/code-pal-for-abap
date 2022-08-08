[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Prefer Pragmas to Pseudo Comments](prefer-pragmas-to-pseudo-comments.md)

## Prefer Pragmas to Pseudo Comments

### What is the intent of the check?

In cases where pragmas are possible (i.e. for findings from the ABAP syntax check and from the Extended Program Check (SLIN)), they should be preferred over pseudo comments as they are more precise with respect to the statement they refer to since they are placed inside of the statement instead of after it.

### How to solve the issue?

Change the `"#EC ` of the pseudo comment to `##` and move the new pragma inside of the statement it refers to if the pseudo comment was placed after the terminating period.

### What to do in case of exception?

This check has no associated pseudo comment or pragma because pseudo comments can only refer to statements, not to other comments.

### Example

Before the check:

```abap
  DATA a TYPE string. "#EC NEEDED
```

After the check:

```abap
  DATA a TYPE string ##NEEDED.
```

### Further Readings & Knowledge

* [Clean ABAP - Prefer pragmas to pseudo comments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-pragmas-to-pseudo-comments)
* [ABAP Keyword Documentation: Pseudo Comments for the Extended Program Check](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenpseudo_comment_slin.htm)

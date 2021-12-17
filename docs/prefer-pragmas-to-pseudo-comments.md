[code pal for ABAP](../README.md) > [Prefer Pragmas to Pseudo Comments](prefer-pragmas-to-pseudo-comments.md)

## Prefer Pragmas to Pseudo Comments

### What is the Intent of the Check?

Based on the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-pragmas-to-pseudo-comments):
> Prefer pragmas to pseudo comments to suppress irrelevant warnings and errors identified by the ATC. Pseudo comments have mostly become obsolete and have been replaced by pragmas

:bulb: Only the Pseudo Comments and Pragmas available in the `SLIN_DESC` table are in scope.

:wan: Code Pal does not support Pragmas.

### How to solve the issue?

Change the `"#EC ` (Pseudo Comment) to `##` (Pragma).

### What to do in case of exception?

This Check cannot be exempt.

### Example

Before the check:

```abap
  DATA a TYPE string.   "#EC NEEDED
```

After the check:

```abap
  DATA a TYPE string.   ##NEEDED
```

### Further Readings & Knowledge

* [Clean ABAP: Prefer pragmas to pseudo comments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-pragmas-to-pseudo-comments)
* [ABAP - Keyword Documentation: Pseudo Comments for the Extended Program Check](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenpseudo_comment_slin.htm)

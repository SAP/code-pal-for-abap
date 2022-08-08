[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Avoid DEFAULT KEY](avoid-default-key.md)

## Avoid default table keys

### What is the intent of the check?

Default table keys (declared by the `WITH DEFAULT KEY` clause in a table definition) can lead to obscure mistakes because they ignore numeric data types and they make it the intent of the key unclear. `SORT` and `DELETE ADJACENT` statements without explicit field list will resort to the primary key of the internal table, which in the case of default keys can lead to unexpected results, in particular in combination with `READ TABLE ... BINARY SEARCH` statements.

Therefore, this check searches for internal table definitions that declare a default table key. 

### How to solve the issue?

Either specify the key components explicitly  
```abap
DATA itab1 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
``` 
or resort to `EMPTY KEY` if you don't need a key at all.  
```abap
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```
If you declare an explicit key, also consider whether you actually should use a sorted or hashed key.

See also: [Clean ABAP - Avoid DEFAULT KEY](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-default-key).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC DEFAULT_KEY` which has to be placed after the declaration:

```abap
CLASS-DATA itab1 TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY. "#EC DEFAULT_KEY
```
```abap
DATA itab1 TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY. "#EC DEFAULT_KEY
```
```abap
TYPES: BEGIN OF type1,
         non_unique TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY object,
         default_key TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY, "#EC DEFAULT_KEY'
         empty_key TYPE STANDARD TABLE OF row_type WITH EMPTY KEY,
       END OF type1. 
```

### Example

Before the check:

```abap
DATA itab1 TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

After the check:

```abap
DATA itab1 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```
```abap
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

### Further Readings & Knowledge

* [Clean ABAP: Avoid DEFAULT KEY](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-default-key)

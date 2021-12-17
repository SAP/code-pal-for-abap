[code pal for ABAP](../README.md) > [Avoid DEFAULT KEY](avoid-default-key.md)

## Avoid DEFAULT KEY

### What is the Intent of the Check?

> Default keys are often only added to get the newer functional statements working. The keys themselves in fact are usually superfluous and waste resources for nothing. They can even lead to obscure mistakes because they ignore numeric data types. The `SORT` and `DELETE ADJACENT` statements without explicit field list will resort to the primary key of the internal table, which in case of usage of `DEFAULT KEY` can lead to very unexpected results when having e.g. numeric fields as component of the key, in particular in combination with `READ TABLE ... BINARY` etc.

Source: [Clean ABAP - Avoid DEFAULT KEY](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-default-key).

Therefore, this check searches for the internal table definitions which forces the use of the default key. 

### How to solve the issue?

> Either specify the key components explicitly  
> ```abap
> DATA itab1 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
> ``` 
> or resort to `EMPTY KEY` if you don't need a key at all.  
> ```abap
> DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
> ```

Source: [Clean ABAP - Avoid DEFAULT KEY](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-default-key).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC DEFAULT_KEY` which has to be placed after the declaration:

```abap
CLASS-DATA itab1 TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY. "#EC DEFAULT_KEY
```
```abap
DATA itab1 TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY. "#EC DEFAULT_KEY
```
```abap
TYPES: BEGIN OF type1, ' )
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

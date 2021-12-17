[code pal for ABAP](../README.md) > [COLLECT restriction](collect.md)

## COLLECT restriction

### What is the Intent of the Check?

The [ABAP - Keyword Documentation](https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/abencollect_guidl.htm?file=abencollect_guidl.htm) says:
> **Rule**
>
> Do not fill standard tables with collections of lines
> 
> Only use the statement COLLECT for hashed tables or sorted tables with a unique key. Do not use it any more for standard tables.
> 
> **Details**
>
> The statement COLLECT is based on unique entries with respect to the primary key and stable key administration. This means that not all categories of internal tables are suitable for COLLECT:
>
> * If the statement COLLECT is applied to a standard table, this table first needs its own internal hash administration. Change operations on the table can invalidate this temporary hash administration. After a change operation of this type, the following COLLECT statements must resort to a linear search, which can affect performance considerably. The primary key of a standard table is also never unique.
> * COLLECT can be used for sorted tables and hashed tables without any problems since these, unlike standard tables, always have a separate, stable key administration that can be utilized by COLLECT. COLLECT can work properly for sorted tables only if the primary key is unique. If a sorted table has a non-unique key, only COLLECT can be used to fill the table, which is difficult to guarantee. In hashed tables, the key values are always unique.

### How does the check work?

It searches for `COLLECT` in:
* Internal tables typed as `SORTED TABLE` with `NON-UNIQUE KEY`;
* Internal tables typed as `STANDARD TABLE`;

### How to solve the issue?

Change the internal table to `SORTED` or `HASHED` as recommended, or perform the collection manually.   

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC COLLECT` which should be placed after the `COLLECT` statement: 

```abap
  COLLECT entry INTO table. "#EC COLLECT
```

### Further Readings & Knowledge

* [ABAP - Keyword Documentation](https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/abencollect_guidl.htm?file=abencollect_guidl.htm)

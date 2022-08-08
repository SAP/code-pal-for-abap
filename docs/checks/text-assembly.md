[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Text Assembly](text-assembly.md)

## Text Assembly

### What is the Intent of the Check?

This check searches for locations where text is assembled from different fragments and string templates (marked by `|`) are not used.

### How to solve the issue?

Use string templates to assemble text from smaller pieces.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC TEXT_ASSEMBLY` which has to be placed after the statement:

```abap
  DATA(first) = 'A'.
  DATA(second) = 'B'. 
  DATA(third) = 'C'.
  
  WRITE first && ': ' && second && ' - ' && third. "#EC TEXT_ASSEMBLY 
```

### Example

Before the check:

```abap
  DATA(first) = 'A'.
  DATA(second) = 'B'. 
  DATA(third) = 'C'.
  
  WRITE first && ': ' && second && ' - ' && third.
```

After the check:

```abap
  DATA(first) = 'A'.
  DATA(second) = 'B'. 
  DATA(third) = 'C'.

  WRITE |{ first }: { second } - { third }|. 
```

### Further Readings & Knowledge

* [Clean ABAP: Use | to assemble text](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use--to-assemble-text)

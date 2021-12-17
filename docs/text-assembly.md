[code pal for ABAP](../README.md) > [Text Assembly](text-assembly.md)

## Text Assembly

### What is the Intent of the Check?

Following the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use--to-assemble-text), this check searches for text assembly and suggests the usage of the `|` to perform it.

### How to solve the issue?

Use `|` to assemble the text

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

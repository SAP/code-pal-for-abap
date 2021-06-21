[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Text Assembly](text-assembly.md)

## Text Assembly

### What is the Intent of the Check?

Following the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use--to-assemble-text) style guide, this check searches for text assembly and suggests the usage of the `|` to perform it.

### How to solve the issue?

Use `|` to assemble text

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC TEXT_ASSEMBLY` which has to be placed after the statement:

```abap
  DATA(first) = 'This'.
  DATA(second) = 'is'. 
  DATA(third) = 'an example'.
  
  WRITE first && ' - ' && second && ' ~ ' && third. "#EC TEXT_ASSEMBLY 
```

### Example

Before the check:

```abap
  DATA(first) = 'This'.
  DATA(second) = 'is'. 
  DATA(third) = 'an example'.
  
  WRITE first && ' - ' && second && ' ~ ' && third.
```

After the check:

```abap
  DATA(first) = 'This'.
  DATA(second) = 'is'. 
  DATA(third) = 'an example'.

  WRITE |{ first } { second } { third }|. 
```

### Further Readings & Knowledge

* [ABAP Styleguides on Clean Code: Use | to assemble text](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use--to-assemble-text)

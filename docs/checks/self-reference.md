[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Self-Reference](self-reference.md)

## Self-Reference

### What is the intent of the check?

This check searches for unnecessary usages of explicitly self-references using `me->` to qualify an instance variable or method. Since this self-reference is implicitly set by the system, it should be omitted.

### How to solve the issue?

Omit the self-reference.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC SELF_REF` which should to be placed after the self-referenced statement:

```abap
  DATA(sum) = me->aggregate_values( values ). "#EC SELF_REF
```

### Example

Before the check:

```abap
  DATA(sum) = me->aggregate_values( values ).
```

After the check:

```abap
  DATA(sum) = aggregate_values( values ).
```

### Further Readings & Knowledge

* [Clean ABAP - Omit self-references](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-the-self-reference-me-when-calling-an-instance-attribute-or-method)

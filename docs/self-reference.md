[code pal for ABAP](../README.md) > [Self-Reference](self-reference.md)

## Self-Reference

### What is the Intent of the Check?

This check searches for the usage of self-reference `me->`. Since this self-reference is implicitly set by the system, you should omit it when calling an instance method.

### How to solve the issue?

Omitting the self-reference whenever calling an instance method.

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

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-the-self-reference-me-when-calling-an-instance-method)

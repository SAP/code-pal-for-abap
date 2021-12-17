[code pal for ABAP](../README.md) > [CALL METHOD Usage Check](call-method-usage.md)

## CALL METHOD Usage Check

### What is the Intent of the Check?

This check verifies the usage of the `CALL METHOD` ABAP statement. It is preferable to call a method dynamically instead of via this statement: CALL METHOD. In other words, prefer a functional instead of a procedural call.

### How does the check work?

It checks the usage of `CALL METHOD` statement in the code.

### How to solve the issue?

Change the long method calls using `CALL METHOD` statement to short method calls using parenthesis notation (dynamic call).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CALL_METH_USAGE` which has to be placed after the `CALL METHOD` statement:

```abap
CALL METHOD method_name. "#EC CALL_METH_USAGE
```

### Example

Before the check:

```abap
DATA(class) = NEW object( ).
CALL METHOD class->method.
```

After the check:

```abap
DATA(class) = NEW object( ).
class->method( ).
```

### Further Readings & Knowledge

* [Clean ABAP: Avoid CALL METHOD Statement](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-calls)

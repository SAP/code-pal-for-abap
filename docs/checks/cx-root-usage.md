[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [CX_ROOT Usage Check](cx-root-usage.md)

## CX_ROOT Usage Check


### What is the intent of the check?

This check searches for exceptions of type `CX_ROOT` (and not one of its subtypes) being directly used in the `CATCH` clause of a `TRY...CATCH` block. Catching all exceptions is often a stop-gap solution during prototyping that should be removed before the code goes productive. 

### How to solve the issue?

Since static and dynamic exceptions must be explicitly declared in method signatures if a method can throw them, you can always explicitly list the types of these exceptions in your `CATCH` clause instead. If you want to really catch even the *unexpected* exceptions, adding `CX_NO_CHECK` as a caught type is equivalent to using `CX_ROOT` and expresses your intent more clearly.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC NEED_CX_ROOT` which should be placed after the `CATCH` statement: 

```abap
TRY.
  cls=>meth( ).
CATCH cx_root.   "#EC NEED_CX_ROOT
  cl_demo_output=>display( 'Catching exception' ).
ENDTRY.
```

An identical check is part of the Extended Program Check (SLIN) delivered by SAP (and existed long before this Code Pal check). Use either the SLIN check *or* this one, since otherwise you will get two different findings for the same issue, and the pragma that suppresses the SLIN check will not suppress this one, and vice versa the pseudo comment that suppresses this check will not suppress the SLIN check.

### Further readings

 - [Clean ABAP - Exceptions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#exceptions)
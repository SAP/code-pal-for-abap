[code pal for ABAP](../README.md) > [CX_ROOT Usage Check](cx-root-usage.md)

## CX_ROOT Usage Check

### What is the Intent of the Check?

This check searches for direct "CX_ROOT" exceptions being used in the code (e.g.: In a TRY-CATCH block). 

### How does the check work?

It search for the "direct" usage of CX_ROOT exceptions like:

Explicitly:
```abap
TRY.
  cls=>meth( ).
CATCH cx_root.   
  cl_demo_output=>display( 'Catching exception' ).
ENDTRY.
```

Implicitly:
```abap
CLASS cx_my_exception DEFINITION INHERITING FROM cx_root.
ENDCLASS.
```

### How to solve the issue?

The solution is to use well defined and specific class-based exceptions.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC NEED_CX_ROOT` which should be placed after the CATCH statement: 

```abap
TRY.
  cls=>meth( ).
CATCH cx_root.   "#EC NEED_CX_ROOT
  cl_demo_output=>display( 'Catching exception' ).
ENDTRY.
```

### Further Readings & Knowledge

* [Clean ABAP: Using class based exceptions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-class-based-exceptions)


[code pal for ABAP](../README.md) > [Non-Class Exception Usage Check](non-class-exception.md)

## Non-Class Exception Check Usage

### What is the Intent of the Check?

This check searches for non-class based exceptions raised in your code. 

### How does the check work?

It search for non class-based exceptions like:

* `RAISE SYSTEM-EXCEPTIONS`
* `RAISE` ( without `EXCEPTION` or `RESUMABLE EXCEPTION` )
* `MESSAGE with RAISING`

### How to solve the issue?

The solution is to use class-base exceptions like:

* `RAISE EXCEPTION`
* `RAISE RESUMABLE EXCEPTION`
* `RAISE SHORTDUMP`
* `THROW` ( in conditions )

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC NON_CL_EXCEPT`:

```abap
RAISE SYSTEM-EXCEPTIONS.  "#EC NON_CL_EXCEPT

RAISE ex_name. "#EC NON_CL_EXCEPT

MESSAGE msg_name RAISING ex_name. "#EC NON_CL_EXCEPT
```

### Further Readings & Knowledge

* [Clean ABAP - Use class based exceptions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-class-based-exceptions)

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Non-Class Exception Usage Check](non-class-exception.md)

## Non-Class Exception Check Usage

### What is the intent of the check?

This check searches locations where "classic" (i.e. not object-oriented) exceptions are raised. 

### How does the check work?

The check searches for the following statements:

* `RAISE` (without `EXCEPTION` or `RESUMABLE EXCEPTION`)
* `MESSAGE with RAISING`

### How to solve the issue?

The solution is to use class-based exceptions instead:

* `RAISE (RESUMABLE) EXCEPTION`
* `RAISE RESUMABLE EXCEPTION`
* `RAISE SHORTDUMP`
* `THROW` (in conditions)

Solely replacing the raising statement is usually not enough. You should also adjust the signature of the procedure to remove the "classic" exceptions and add the new class-based exceptions in a `RAISING` clause unless they are `CX_NO_CHECK` exceptions.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC NON_CL_EXCEPT`.

Note that this check is a subset of a similar check in the Extended Program Check (SLIN) delivered by SAP. That check accepts no pseudo comments or pragmas and places its findings at the location of definition of the classic exception, i.e. the `METHOD method_name EXCEPTIONS exception_name` declaration. We recommend that you *either* use this Code Pal check *or* the corresponding SLIN check, but not both, since if you use both you get two findings for the exact same issue.

```abap
RAISE SYSTEM-EXCEPTIONS.  "#EC NON_CL_EXCEPT

RAISE ex_name. "#EC NON_CL_EXCEPT

MESSAGE msg_name RAISING ex_name. "#EC NON_CL_EXCEPT
```

### Further Readings & Knowledge

* [Clean ABAP - Use class based exceptions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-class-based-exceptions)

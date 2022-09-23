[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Number of Executable Statements Check](number-executable-statements.md)

## Number of Executable Statements Check

### What is the intent of the check?

This check counts the number of executable ABAP statements per modularization unit and reports a finding when this exceeds a configurable threshold. If there are too many statements in a code block, this can be an indicator that the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) (SRP) is violated or that the code mixes different abstraction levels.

### How does the check work?

The check counts the number of executable ABAP statements, i.e. statement that are executed at runtime. This excludes statements like variable declarations that do not correspond to an action at runtime. 

```abap
DATA var TYPE i.
var = 1.
DATA(var_2) = 2.
```

The first statement is not executable, while the second and third are.

### How to solve the issue?

Modularize your code by extracting smaller methods that each adhere to the SRP. Also, consider grouping these smaller methods in their own classes instead of having a lot of methods in the same class.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_NOES` which should be placed right after the `ENDMETHOD` statement.

Note that this check is equivalent to a subset of the "Procedural Metrics" check delivered by SAP. That check accepts no pseudo comments or pragmas. We recommend that you *either* use this Code Pal check *or* the SAP-delivered check, but not both, since if you use both you get two findings for the exact same issue.

```abap
METHOD method_name.
  " ...
ENDMETHOD. "#EC CI_NOES
```

### Further Reading

 - [Clean ABAP - Keep methods small](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#keep-methods-small)

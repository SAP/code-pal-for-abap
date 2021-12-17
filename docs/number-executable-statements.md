[code pal for ABAP](../README.md) > [Number of Executable Statements Check](number-executable-statements.md)

## Number of Executable Statements Check

### What is the Intent of the Check?

This check counts the number of non-declarative ABAP Statements per modularization unit up to a maximum. If there are too many statements in a code block, it is an indicator that the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle) is violated.

A high number of executable statements is an indicator that the source code might be not readable. In addition, the risk of introducing bugs is higher with a high number of executable statements in the code.

### How does the check work?

The check counts the number of non-declarative ABAP Statements, that is ABAP Statements without data declarations, program introduction statements, etc.

### How to solve the issue?

Modularize your code. Follow the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle). Split the method into other smaller methods (create service classes whenever possible).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_NOES` which should be placed right after the `ENDMETHOD` statement:

```abap
METHOD method_name.
  " ...
ENDMETHOD. "#EC CI_NOES
```

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Cyclomatic Complexity Check](cyclomatic-complexity.md)

## Cyclomatic Complexity Check

### What is the intent of the check?

This check measures the [cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) of your code based on a control flow graph. It counts the number of independent possible paths through the source code.

A high value in cyclomatic complexity is an indicator that the source code is rather complex and might be difficult to read and understand. Thus, maintaining and extending this complex code might also be difficult, in particular since the high complexity means that there are many different paths through the code that are very likely to not all be well-tested.

### How does the check work?

In this implementation, the number of binary decision points `b` is counted as follows:

 - Every `IF`, `ELSEIF`, `CHECK` and `ASSERT` statement is a decision point.
 - Within a `CASE` structure every `WHEN` branch is a decision point, unless it is the `WHEN OTHERS` branch.
 - Loops (`LOOP`, `DO`, `WHILE`) are decision points since they might be executed at least once or not at all. `DO` loops are only decision points when they have a `TIMES` addition, since otherwise they cannot branch.

 The cyclomatic complexity of the code is `b+1`.

### How to solve the issue?

Modularize the functionality into smaller blocks. This reduces the cyclomatic complexity and increases the readability.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_CYCLO` which should be placed after the `ENDMETHOD` statement:

```abap
METHOD method_name.
  " Method content
ENDMETHOD. "#EC CI_CYCLO
```

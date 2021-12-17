[code pal for ABAP](../README.md) > [Cyclomatic Complexity Check](cyclomatic-complexity.md)

## Cyclomatic Complexity Check

### What is the Intent of the Check?

This check measures the complexity of your code based on a control flow graph. It counts the number of linearly-independent possible paths through the source code.

A high value in cyclomatic complexity is an indicator that the source code might have a difficult readability. Thus, maintaining and extending this complex code is also difficult. In addition, the risk of introducing bugs (regression) is higher in code with high cyclomatic complexity.

### How does the check work?

In this implementation, the number of binary decision points "b" (for instance: IF-statements) is counted.

The cyclomatic complexity  M = b + 1  is calculated as follows:

Every IF and ELSEIF are counted as decisions ("b") - but not the "AND" and "OR" and other logical operands within them.
Within a CASE statement all WHEN tokens are counted ("b").
LOOP, DO and WHILE are all counted ("b") and also CHECK within a loop as this is a conditional short circuit of the loop (avoid using CHECK, try to use an IF-Statement instead).
FORM, METHOD and FUNCTION are counted ("b") as these are separate sections of code with distinct entry and exit points. 
Within SQL statements a WHERE clause is also a decision point ("b") and is therefore counted in SELECT, MODIFY, UPDATE and DELETE statements, however, these statements are not counted if there is no WHERE clause. Similarly a KEY clause in a READ statement is counted ("b").
A coding block without decision points (for instance: just some statements of DATA declaration or some APPENDs in tables) will have at least M = 1.

### How to solve the issue?

Modularize the functionality into smaller blocks. This reduces the cyclomatic complexity and increases the readability.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_CYCLO` which should be placed after the `ENDMETHOD` statement:

#### Example

```abap
METHOD method_name.
  " Method content
ENDMETHOD. "#EC CI_CYCLO
```

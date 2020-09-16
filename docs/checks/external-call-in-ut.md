[BACK](../check_documentation.md)

# External Call in Unit Tests Check
## What is the Intent of the Check?
The “External Call within Unit-Test” Check scans test classes and its contents searching for any kind of explicit redirection (external call changing the main workflow to another program) within the tests. Since every external call/redirection is considered to be a dependency, this should not be allowed in test code.

## How does the check work?
Statements like: SUBMIT which deviates completely the workflow (callstack) of a program are detected in test code.

## Which attributes can be maintained?
![Attributes](./img/external_call_in_ut.png)

## How to solve the issue?
The solution is to mock these external call/redirection with a proper dependency isolation technique.

## What to do in case of exception?
In special cases, it is possible to suppress a finding by using the pseudo comment “#EC EXT_CALL_UT. The pseudo comment must be placed right after the SUBMIT statement itself.

## Example
```abap
SUBMIT XXXXX.       “#EC EXT_CALL_UT
```

[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [External Call in Unit Tests Check](external-call-in-ut.md)

## External Call in Unit Tests-Check

### What is the Intent of the Check?

This check finds any kind of explicit redirection (external call changing the main workflow to another program) within test methods of ABAP Unit test classes. Since every external call or redirection is considered to be a dependency, they should not be present in test code.

### How does the check work?

The check searches for statements which may lead to an external call or redirection (e.g. `SUBMIT`) diverting the regular workflow (call stack) of a program to another program. The check also searches for any Remote Function Calls (RFCs) as well as usages of `CL_GUI_*` classes.

### How to solve the issue?

The solution is either to remove or mock these external calls and redirections with a proper dependency isolation technique.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC EXT_CALL_UT` which should be placed right after the statement itself:

```abap
SUBMIT program.       “#EC EXT_CALL_UT
```

### Further Readings & Knowledge

* [Unit testing with ABAP unit](https://help.sap.com/docs/SAP_S4HANA_CLOUD/25cf71e63940453397a32dc2b7676947/08c60b52cb85444ea3069779274b43db.html?q=abap%20unit%20test)

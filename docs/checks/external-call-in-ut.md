[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [External Call in Unit Tests Check](external-call-in-ut.md)

## External Call in Unit Tests-Check

### What is the intent of the check?

This check searches test classes for statements that change the current main program or cause parallel sessions to spawn. Test code should not change the control flow to other main programs or directly call GUI elements.

### How does the check work?

The check searches for `SUBMIT` statements, remote function calls of any kind (`CALL FUNCTION ... DESTINATION`) and calls to `CL_GUI_*` classes.

### How to solve the issue?

Remove these external calls and/or mock them with a proper dependency isolation technique.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `“#EC EXT_CALL_UT` which should be placed right after the statement itself:

```abap
SUBMIT program.       “#EC EXT_CALL_UT
```

### Further Readings & Knowledge

* [ABAP Test Tools & Frameworks](https://pages.github.tools.sap/EngineeringCulture/ase/ABAP/abapTestTools.html)

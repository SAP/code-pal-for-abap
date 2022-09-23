[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Message Translation](message-translation.md)

## Message Translation

### What is the intent of the check?

This check finds `MESSAGE` statements where an untranslatable text literal is specified instead of a text element or message from a message class. 

### How to solve the issue?

Use [message classes](https://help.sap.com/doc/saphelp_nw75/7.5.5/en-us/4e/c242f66e391014adc9fffe4e204223/content.htm) or [text elements](https://help.sap.com/doc/saphelp_nw73ehp1/7.31.19/en-US/e3/9609f6eb0711d194d100a0c94260a5/content.htm) instead.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC MSG_TRANSL` which has to be placed after the `MESSAGE` statement.

Note that this check is a subset of a similar check in the Extended Program Check (SLIN) delivered by SAP. That check accepts a pragma `##NO_TEXT` for suppressing its findings that Code Pal cannot evaluate (pragmas are inaccessible to ordinary Code Inspector checks). We recommend that you *either* use this Code Pal check *or* the corresponding SLIN check, but not both, since if you use both you get two findings for the exact same issue.

```abap
  MESSAGE 'File not found!' TYPE 'W'. "#EC MSG_TRANSL
```

### Example

Before the check:

```abap
  MESSAGE 'File not found!' TYPE 'W'.
```

After the check:

```abap
  MESSAGE i002(00).
```

or 

```
  MESSAGE TEXT-001 TYPE 'W'.
```

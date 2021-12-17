[code pal for ABAP](../README.md) > [Message Translation](message-translation.md)

## Message Translation

### What is the Intent of the Check?

It identifies the `MESSAGES` statement where the text is hard-coded as it cannot be translated.

### How to solve the issue?

Use [Message Classes](https://help.sap.com/doc/saphelp_nw75/7.5.5/en-us/4e/c242f66e391014adc9fffe4e204223/content.htm) or [Text Elements](https://help.sap.com/doc/saphelp_nw73ehp1/7.31.19/en-US/e3/9609f6eb0711d194d100a0c94260a5/content.htm) instead.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC MSG_TRANSL` which has to be placed after the `MESSAGE` statement:

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
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno.
  MESSAGE TEXT-001 TYPE 'W'.
```

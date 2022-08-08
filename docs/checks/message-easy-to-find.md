[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Message Easy To Find](message-easy-to-find.md)

## Message Easy To Find

### What is the intent of the check?

Since the specification of message classes and numbers by variables creates no entry in the where-used list of the corresponding message, this check searches for `MESSAGE` statements that do not statically specify their message by literals since these cannot be found in the where-used list of the message.

### How to solve the issue?

Specify the message used by literals.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC MSG_FIND` which has to be placed after the message statement:

```abap
  DATA(message_class) = '00'.
  MESSAGE i002(message_class). "#EC MSG_FIND
```

### Example

Before the check:

```abap
  DATA(message_class) = '00'.
  DATA(message_id) = '002'.

  MESSAGE i002(message_class).
  MESSAGE ID message_class type 'I' NUMBER message_id.
```

After the check:

```abap
  MESSAGE i002(00).
  MESSAGE ID 00 type 'I' NUMBER 002.
```

### Further Readings & Knowledge

* [Clean ABAP - Make messages easy to find](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#make-messages-easy-to-find)

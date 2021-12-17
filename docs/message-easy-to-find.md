[code pal for ABAP](../README.md) > [Message Easy To Find](message-easy-to-find.md)

## Message Easy To Find

### What is the Intent of the Check?

Based on the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#make-messages-easy-to-find), it searches for the `MESSAGE` statement that cannot be detected if you run a where-used search in the transaction `SE91`.

### How to solve the issue?

Declare the message class instead of making it dynamic. 

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
  MESSAGE ID '00' type 'I' NUMBER '002'. 
```

After the check:

```abap
  MESSAGE i002(00).
  MESSAGE ID 00 type 'I' NUMBER 002.
```

### Further Readings & Knowledge

* [Clean ABAP: Make messages easy to find](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#make-messages-easy-to-find)

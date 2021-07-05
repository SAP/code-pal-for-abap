[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Undetectable Message Statement](undetectable-message-statement.md)

## Undetectable Message Statement

### What is the Intent of the Check?

The [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#make-messages-easy-to-find) style guide says to make the message easy to find through a where-used search from transaction `SE91`.

:bulb: `sy-msgid` and `sy-msgno` are exempt.  
:bulb: `MESSAGE` in the `catch` block is exempt.  
:bulb: Hardcoded `MESSAGE` is exempt.  
:bulb: Text Elements are exempt.  

### How to solve the issue?

Declare the message class instead of making it dynamic. 

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC UNDETEC_MSG` which has to be placed after the message statement:

```abap
  DATA(message_class) = '00'.
  MESSAGE i002(message_class). "#EC UNDETEC_MSG
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

* [ABAP Styleguides on Clean Code: Make messages easy to find](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#make-messages-easy-to-find)

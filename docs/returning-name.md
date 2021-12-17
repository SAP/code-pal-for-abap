[code pal for ABAP](../README.md) > [Returning Name](returning-name.md)

## Returning Name

### What is the Intent of the Check?

Good/proper method names are usually so meaningful, that the `RETURNING` parameter does not need a name of its own.  
The name would do little more than parrot the method name or repeat something obvious.

### How to solve the issue?

Calling the `RETURNING` parameter as `RESULT`.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC RET_NAME` which should be placed after the method declaration:

```abap
  METHODS get_name
    RETURNING
      VALUE(name) TYPE string. "#EC RET_NAME
```

### Example

Before the check:

```abap
  METHODS get_name
    RETURNING
      VALUE(name) TYPE string.
```

After the check:

```abap
  METHODS get_name
    RETURNING
      VALUE(result) TYPE string.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#consider-calling-the-returning-parameter-result)

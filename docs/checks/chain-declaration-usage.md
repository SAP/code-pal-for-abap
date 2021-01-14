[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Chain Declaration Usage](chain-declaration-usage.md)

## Chain Declaration Usage

### What is the Intent of the Check?

This check verifies the usage of chain up-front declarations.

### How to solve the issue?

Change the chain up-front declarations to inline declarations.

### What to do in case of exception?

In exceptional cases, you can suppress this finding using the pseudo comment `"#EC CHAIN_DECL_USAG` which should be placed after the `DATA:` statement.

```abap
  DATA: "#EC CHAIN_DECL_USAG
    string TYPE string,
    json TYPE REF TO cl_abap_json,
    client LIKE sy-mandt.
```

### Example

Before the check:

```abap
  DATA:
    string TYPE string,
    json TYPE REF TO cl_abap_json,
    client LIKE sy-mandt.
```

After the check:

```abap
  DATA string TYPE string.
  DATA json TYPE REF TO cl_abap_json.
  DATA client LIKE sy-mandt.
```

### Further Readings & Knowledge

* [ABAP Styleguides on Clean Code: Do not chain up-front declarations](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#do-not-chain-up-front-declarations)

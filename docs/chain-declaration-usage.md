[code pal for ABAP](../README.md) > [Chain Declaration Usage](chain-declaration-usage.md)

## Chain Declaration Usage

### What is the Intent of the Check?

This check verifies the usage of chain up-front declarations.

### How to solve the issue?

Change the chain up-front declarations to inline declarations.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CHAIN_DECL_USAG` which should be placed after the declaration statement:

```abap
  DATA: "#EC CHAIN_DECL_USAG
    string TYPE string,
    json TYPE REF TO cl_abap_json,
    client LIKE sy-mandt.
```

```abap
  TYPES: "#EC CHAIN_DECL_USAG
    name TYPE string,
    json TYPE REF TO cl_abap_json.
```

```abap
  CONSTANTS: "#EC CHAIN_DECL_USAG
    min_age TYPE i VALUE 18,
    min_name_size TYPE i VALUE 3.
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

Or even (which looks neat - but it won't be enforced):

```abap
DATA var1         TYPE a.
DATA var2         TYPE string.
DATA my_var3      TYPE int.
DATA a            TYPE c.
```

### Further Readings & Knowledge

* [Clean ABAP: Do not chain up-front declarations](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#do-not-chain-up-front-declarations)

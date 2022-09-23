[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Chain Declaration Usage](chain-declaration-usage.md)

## Chain Declaration Usage

### What is the intent of the check?

This check searches for chained up-front declarations of variables. Chaining visually implies a logical grouping that is often not actually present and variables often should rather be declared inline at their first point of usage.

### How to solve the issue?

Change the chained up-front declarations to inline declarations.

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
or 
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
  string = `Hello world`.
  create object json
    exporting iv_json = string.
  client = sy-mandt.     
```

After the check:

```abap
  data(string) = `Hello world`.
  data(json) = new cl_abap_json( string ).
  data(client) = sy-mandt. 
```

or 

```abap
  DATA string TYPE string.
  DATA json TYPE REF TO cl_abap_json.
  DATA client LIKE sy-mandt.
  string = `Hello world`.
  create object json
    exporting iv_json = string.
  client = sy-mandt.   
```

### Further Readings & Knowledge

* [Clean ABAP: Do not chain up-front declarations](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#do-not-chain-up-front-declarations)

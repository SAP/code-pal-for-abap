[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [TEST-SEAM Statement Usage Check](test-seam-usage.md)

## TEST-SEAM Statement Usage Check

### What is the intent of the check?

This check searches for the usage of the ABAP statement `TEST-SEAM`. Test seams are invasive and tend to get entangled in private dependencies, so that they are hard to keep alive and stable in the long run.

### How does the check work?

This check searches for usage of the `TEST-SEAM` statement.

## How to solve the issue?

`TEST-SEAM` shall not be used. Refactor the code so that a proper isolation technique (like mocks) can be used.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC TEST_SEAM_USAGE` which should be placed right after the declaration.

### Example

```abap
TEST-SEAM seam_name.    "#EC TEST_SEAM_USAGE
```

### Further Readings & Knowledge

* [Clean ABAP - Avoid Usage of TEST-SEAM](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-test-seams-as-temporary-workaround)
* [Unit testing with ABAP unit](https://help.sap.com/docs/SAP_S4HANA_CLOUD/25cf71e63940453397a32dc2b7676947/08c60b52cb85444ea3069779274b43db.html?q=abap%20unit%20test)


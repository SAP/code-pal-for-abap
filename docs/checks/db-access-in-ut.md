[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [DB Access in Unit Tests Check](db-access-in-ut.md)

## Database Access within Unit-Tests Check

### What is the intent of the check?

The check searches for database operations inside ABAP Unit test classes that do not use an SQL test double framework to mock the database. Unit tests should usually be isolated from the actual database.

### How does the check work?

The check behaves differently depending on the test class's risk level:

#### 1. `RISK LEVEL HARMLESS` or not set

The check reports:
* `SELECT`
* `INSERT`
* `UPDATE`
* `MODIFY`
* `DELETE`
* `ROLLBACK`
* `COMMIT`
* `ALTER`

#### 2. `RISK LEVEL DANGEROUS` or `RISK LEVEL CRITICAL`

The check reports:
* `UPDATE`
* `MODIFY`
* `DELETE`
* `ALTER`

#### 3. Test Frameworks

If an instance of one of the following types associated with a test double framework is used in the test class, no findings are reported for database accesses:
* `IF_OSQL_TEST_ENVIRONMENT`
* `CL_OSQL_TEST_ENVIRONMENT`
* `IF_CDS_TEST_ENVIRONMENT`
* `CL_CDS_TEST_ENVIRONMENT` 
  
### How to solve the issue?

Isolate the database dependency using one of the test double frameworks for SQL and CDS accesses:
* [ABAP SQL Test Double Framework](https://help.sap.com/viewer/c238d694b825421f940829321ffa326a/1809.000/en-US/8562b437073d4b9c93078c45f7a64f21.html)
* [ABAP CDS Test Double Framework](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/8562b437073d4b9c93078c45f7a64f21.html)

### What to do in case of exception?

In special cases, it is possible to suppress a finding by using the pseudo comment `"#EC DB_ACCESS_UT`.  
The pseudo comment must be placed right after the statement that accesses the database.

```ABAP
SELECT * FROM tadir INTO TABLE @DATA(entries).       "#EC DB_ACCESS_UT
```

### Further Readings & Knowledge

* [Clean ABAP - Avoid Usage of TEST-SEAM](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-test-seams-as-temporary-workaround)
* [Unit testing with ABAP unit](https://help.sap.com/docs/SAP_S4HANA_CLOUD/25cf71e63940453397a32dc2b7676947/08c60b52cb85444ea3069779274b43db.html?q=abap%20unit%20test)


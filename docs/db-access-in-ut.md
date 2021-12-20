[code pal for ABAP](../README.md) > [DB Access in Unit Tests Check](db-access-in-ut.md)

## Database Access within Unit-Tests Check

### What is the Intent of the Check?

The Check identifies database operations within the test classes. 

### How does the check work?

The Check behaves differently depending on the test class classification (`RISK LEVEL`) and automatically exempts the finding if a known test framework is find. 

#### 1. `RISK LEVEL HARMLESS` or not set

The Check reports:
* `SELECT`
* `INSERT`
* `UPDATE`
* `MODIFY`
* `DELETE`
* `ROLLBACK`
* `COMMIT`
* `ALTER`

#### 2. `RISK LEVEL DANGEROUS` or `RISK LEVEL CRITICAL`

The Check reports:
* `UPDATE`
* `MODIFY`
* `DELETE`
* `ALTER`

In short, the Check allows:
* `SELECT`
* `INSERT`
* `COMMIT`
* `ROLLBACK`

#### 3. Test Frameworks

The Check identifies if the test class uses one of the following objects internally and exempts the finding automatically.

The relevant objects for the automatic exemption are:
* `IF_OSQL_TEST_ENVIRONMENT`
* `CL_OSQL_TEST_ENVIRONMENT`
* `IF_CDS_TEST_ENVIRONMENT`
* `CL_CDS_TEST_ENVIRONMENT` 
  
### How to solve the issue?

Please, isolate the database dependency using one of the below frameworks:
* [ABAP SQL Test Double Framework](https://help.sap.com/viewer/c238d694b825421f940829321ffa326a/1809.000/en-US/8562b437073d4b9c93078c45f7a64f21.html)
* [ABAP CDS Test Double Framework](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/8562b437073d4b9c93078c45f7a64f21.html)

### What to do in case of exception?

In special cases, it is possible to suppress a finding by using the pseudo comment `"#EC DB_ACCESS_UT`.  
The pseudo comment must be placed right after the DB access statement.

```ABAP
SELECT * FROM tadir INTO TABLE @DATA(entries).       "#EC DB_ACCESS_UT
```

### Further Readings & Knowledge

* [Clean ABAP - Avoid Usage of TEST-SEAM](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-test-seams-as-temporary-workaround)
* [ABAP Test Tools & Frameworks](https://pages.github.tools.sap/EngineeringCulture/ase/ABAP/abapTestTools.html)


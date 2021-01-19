[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [DB Access in Unit Tests Check](db-access-in-ut.md)

## Database Access within Unit-Tests Check

### What is the Intent of the Check?

This check scans test classes and its contents searching for any kind of explicit DB access within the tests. Since every DB access is considered to be a dependency, this should not be allowed in test code.

### How does the check work?

Statements like: SELECT, EXEC SQL, COMMIT, COMMIT WORK, ROLLBACK, INSERT, DELETE, ALTER; UPDATE or READ TABLE accessing physical database tables (SAP Dictionary Tables) are detected and presented.

### How to solve the issue?

The solution is to mock these DB accesses with a proper dependency isolation technique.

### What to do in case of exception?

In special cases, it is possible to suppress a finding by using the pseudo comment `"#EC DB_ACCESS_UT`.  
The pseudo comment must be placed right after the DB access statement.

### Example

```abap
SELECT XXXXX.       "#EC DB_ACCESS_UT
```
### Risk Level
Definition
The RISK LEVEL describes the effects that a test can have on the data security of the system:

•	CRITICAL - such as changes to system settings or Customizing;

•	DANGEROUS - such as changes to persistent data;

•	HARMLESS - no effects on persistent data or system settings.


The RISK LEVEL is assigned by the following extension to the CLASS DEFINITION statement of a test class:

•	RISK LEVEL CRITICAL - default;
•	RISK LEVEL DANGEROUS;
•	RISK LEVEL HARMLESS.

### Duration
Definition
The DURATION property allows you to define an expected runtime for all methods of a test class.

At runtime the cumulated duration of all test methods is measured. If the actual duration exceeds the expectation ABAP Unit will raise an alert.
Please not the cumulated duration contains also system activities as program compilation. In case of doubts it is recommend to choose the higher category.

Maintenance
Use on of the following key words in the CLASS DEFINITON statement of a test class to specify the duration category:

•	DURATION SHORT (default) -within the blink of an eye ( < 10 seconds );
•	DURATION MEDIUM - take a sip of tea ( < 2 minutes );
•	DURATION LONG - get another cup.

Purpose
Unit tests consist of source code and can contain errors. It is easy to recognize syntax errors and runtime errors, but endless loops are more difficult to detect.
With help of this classification of the duration the test runner is able to detect these situations and cancel the execution.

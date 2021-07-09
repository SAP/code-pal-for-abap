[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [DB Access in Unit Tests Check](db-access-in-ut.md)

## Database Access within Unit-Tests Check

### What is the Intent of the Check?

This check scans test classes and its contents searching for any kind of explicit DB access within the tests. Since every DB access is considered to be a dependency, this should not be allowed in test code.

### How does the check work?

Statements like: SELECT, EXEC SQL, COMMIT, COMMIT WORK, ROLLBACK, INSERT, DELETE, ALTER; UPDATE or READ TABLE accessing physical database tables (SAP Dictionary Tables) are detected and presented. However, the Check acts differently according to the RISK LEVEL classification of the test class under evaluation. So that:
	
  · RISK LEVEL HARMLESS or missing RISK LEVEL classification --> These DB Operation will be forbidden/reported: COMMIT, DELETE, INSERT, MODIFY, ROLLBACK, SELECT and UPDATE statements on persistent DDIC tables;
  
  · RISK LEVEL DANGEROUS/CRITICAL --> UPDATE, MODIFY DELETE, COMMIT, ROLLBACK operation(s) on persistent DDIC tables will be forbidden/reported.

REMARK: For more details on RISK LEVEL classification, please refer to the last two sections of this page or follow this path: SE24 --> <Open any Test Class> --> Properties --> Highest Duration Category / Highest Allowed Risk --> PRESS F1.

Besides, test classes having AMDP (this happens only in productive mode) are already excluded from the scope of the check (in other words, exempted) since the Check is only applicable on test-code. Again, data access should be always mocked. For the mocking, one of these alternatives should be used:

  · OSQL Test Framework; and/or

  · CDS Test Framework; and/or

  · Test-Double Framework (isolating the database access in classes an mocking them via TDF).
  
Ideally, there has to be a data access class for every (customizing, master, or transactional) database table. That one shall be tested with one of the mocking frameworks listed above. All backend code layer above this boundary shall be tested with object-oriented test doubles according to the test pyramid strategy. There may be still some non-isolated E2E tests across systems, but these shall not test functionality, rather focus on connectivity, networking and things like that.  
  
### How to solve the issue?

The solution is to mock these DB accesses with a proper dependency isolation technique.

### What to do in case of exception?

In special cases, it is possible to suppress a finding by using the pseudo comment `"#EC DB_ACCESS_UT`.  
The pseudo comment must be placed right after the DB access statement.

### Example

```abap
SELECT XXXXX.       "#EC DB_ACCESS_UT
```
### How to set the "Risk Level"
Definition: The RISK LEVEL describes the effects that a test can have on the data security of the system.
The RISK LEVEL is assigned by the following extension to the CLASS DEFINITION statement of a test class:

•	RISK LEVEL CRITICAL (default) - such as changes to system settings or Customizing;

•	RISK LEVEL DANGEROUS          - such as changes to persistent data;

•	RISK LEVEL HARMLESS           - no effects on persistent data or system settings.


### How to set the "Duration"
Definition: The DURATION property allows you to define an expected runtime for all methods of a test class.

At runtime the cumulated duration of all test methods is measured. If the actual duration exceeds the expectation ABAP Unit will raise an alert.
Please not the cumulated duration contains also system activities as program compilation. In case of doubts it is recommend to choose the higher category.

Maintenance: Use on of the following key words in the CLASS DEFINITION statement of a test class to specify the duration category:

•	DURATION SHORT (default) - within the blink of an eye ( < 10 seconds );

•	DURATION MEDIUM          - take a sip of tea ( < 2 minutes );

•	DURATION LONG            - get another cup.

Purpose: Unit tests consist of source code and can contain errors. It is easy to recognize syntax errors and runtime errors, but endless loops are more difficult to detect.
With help of this classification of the duration the test runner is able to detect these situations and cancel the execution.

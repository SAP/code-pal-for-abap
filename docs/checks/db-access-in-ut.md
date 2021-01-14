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

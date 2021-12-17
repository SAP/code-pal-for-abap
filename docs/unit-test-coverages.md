[code pal for ABAP](../README.md) > [Unit-Test Coverages (Statement, Branch and Procedure)](unit-test-coverages.md)

## Unit-Test Coverages (Statement, Branch and Procedural Coverage)

### What is the Intent of the Check?

This check executes the ABAP Unit-Test framework and returns the current coverage percentile for the object in question. It verifies if the coverage is under the defined thresholds (customized in the checks).

> :WARNING: Use this check with a small set of objects only (due to possible performance issues).

### How to solve the issue?

Improve the Unit-Test coverage(s) by writing Unit-Tests.

### What to do in case of exception?

No exemptions are allowed.

### Further Readings & Knowledge

* [Unit-Testing in ABAP](https://help.sap.com/viewer/c238d694b825421f940829321ffa326a/7.5.19/en-US/4ec18be06e391014adc9fffe4e204223.html)
* [MartinFowler - Test Coverage](https://martinfowler.com/bliki/TestCoverage.html)

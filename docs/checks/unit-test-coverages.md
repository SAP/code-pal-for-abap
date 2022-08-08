[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Unit-Test Coverages (Statement, Branch and Procedure)](unit-test-coverages.md)

## Unit-Test Coverages (Statement, Branch and Procedural Coverage)

### What is the intent of the check?

This check executes the ABAP Unit framework and measures the current coverage percentile for the object in question. It reports a finding if the coverage is below a customizable threshold.

There are three versions of this check for the three different types of coverage that can be measured: Statement coverage, branch coverage and procedure coverage.

### How to solve the issue?

Improve the unit test coverage by writing additional unit tests.

### What to do in case of exception?

No exemptions are allowed.

### Further Readings & Knowledge

* [Unit-Testing in ABAP](https://help.sap.com/viewer/c238d694b825421f940829321ffa326a/7.5.19/en-US/4ec18be06e391014adc9fffe4e204223.html)
* [MartinFowler - Test Coverage](https://martinfowler.com/bliki/TestCoverage.html)

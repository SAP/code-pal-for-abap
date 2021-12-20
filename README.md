# code pal for ABAP

[![abap package version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/SAP/code-pal-for-abap/src/y_code_pal_version.intf.abap/abap&label=version)](changelog.txt)
[![license](https://img.shields.io/github/license/SAP/code-pal-for-abap)](LICENSE)
[![REUSE status](https://api.reuse.software/badge/github.com/SAP/code-pal-for-abap)](https://api.reuse.software/info/github.com/SAP/code-pal-for-abap)

Based on the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md), this tool contains a set of rules to support [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md) adherence.  
Together, we both support you in writing a clean ABAP code. 

❣️ It's **free** and **open-source**.

## Features

- Supports local and remote inspections;
- Supports SAP GUI and Eclipse (ADT);
- Supports ABAP Test Cockpit (ATC) and Code Inspector (SCI);
- Skips generated code; 
- Allows exemptions;
- Allows configuration based on:
  - Rule validity;
  - Rule severity (priority);
  - Rule threshold;
  - Rule relevance for productive and test codes;
  - Rule relevance for objects created since a specific date;
  - Rule relevance for new child objects;
  - Rule relevance for exemptions.

## Set of Checks

- [Avoid DEFAULT KEY](checks/avoid-default-key.md)
- [Boolean Input Parameter](checks/boolean-input-parameter.md)
- [CALL Method Usage](checks/call-method-usage.md)
- [Chain Declaration Usage](checks/chain-declaration-usage.md)
- [CHECK Statement Position](checks/check-statement-position.md)
- [CHECK in LOOP](checks/check-in-loop.md)
- [COLLECT restriction](checks/collect.md)
- [Combination of Output Parameters](checks/method-output-parameter.md)
- [Comment Position](checks/comment-position.md)
- [Comment Type](checks/comment-type.md)
- [Comment Usage](checks/comment-usage.md)
- [Constants Interface](checks/constants-interface.md)
- [Cyclomatic Complexity](checks/cyclomatic-complexity.md)
- [CX_ROOT Usage](checks/cx-root-usage.md)
- [Database Access in Unit-Test](checks/db-access-in-ut.md)
- [Deprecated Classes](checks/deprecated-classes.md)
- [Deprecated Key Word](checks/deprecated-key-word.md)
- [Empty Catch](checks/empty_catch.md)
- [Empty IF Branches](checks/empty-if-branches.md)
- [Empty Procedure](checks/empty-procedure.md)
- [Equals Sign Chaining](checks/equals-sign-chaining.md)
- [External Call in Unit Test](checks/external-call-in-ut.md)
- [FORM Routine Usage](checks/form-routine.md)
- [FUNCTION Module Usage](checks/function-routine.md)
- [Magic Number Usage](checks/magic-number.md)
- [Message Easy To Find](checks/message-easy-to-find.md)
- [Message Translation](checks/message-translation.md)
- [Method Return Bool](checks/method-return-bool.md)
- [Missing Interface](checks/interface-in-class.md)
- [Nesting Depth](checks/maximum-nesting-depth.md)
- [Non-Class Exception Usage](checks/non-class-exception.md)
- [Number of Attributes](checks/number-attributes.md)
- [Number of Events](checks/number-events.md)
- [Number of Executable Statements](checks/number-executable-statements.md)
- [Number of Interfaces](checks/number-interfaces.md)
- [Number of Methods](checks/number-methods.md)
- [Number of Public Attributes](checks/number-public-attributes.md)
- [Number of Output Parameter](checks/number-output-parameter.md)
- [Prefer CASE to ELSEIF](checks/prefer-case-to-elseif.md)
- [Prefer RETURNING to EXPORTING](checks/prefer-returning-to-exporting.md)
- [Prefer IS NOT to NOT IS](checks/prefer-is-not-to-not-is.md)
- [Prefer LINE_EXISTS or LINE_INDEX to READ TABLE or LOOP AT](checks/prefer-line-exists.md)
- [Prefer NEW to CREATE OBJECT](checks/prefer-new-to-create-object.md)
- [Prefer Pragmas to Pseudo Comments](checks/prefer-pragmas-to-pseudo-comments.md)
- [Pseudo Comment Usage](checks/pseudo-comment-usage.md)
- [Omit Optional EXPORTING](checks/omit-optional-exporting.md)
- [Optional Parameters](checks/optional-parameters.md)
- [READ TABLE with Subsequent Memory Assignment](checks/sub-assign-read-table.md)
- [RECEIVING Statement Usage](checks/receiving-usage.md)
- [Returning Name](checks/returning-name.md)
- [Scope of Variable](checks/scope-of-variable.md)
- [Self-Reference](checks/self-reference.md)
- [TEST-SEAM Statement Usage](checks/test-seam-usage.md)
- [Text Assembly](checks/text-assembly.md)
- [Unit-Test Coverages (Statement, Branch and Procedure)](checks/unit-test-coverages.md)
- [Unit-Test Assert Validator](checks/unit_test_assert.md)

## Dependencies

- SAP NetWeaver **7.50** or higher
- [SAP Note 2527903 - Remote analysis (for check system)](https://launchpad.support.sap.com/#/notes/2527903)
- [abapGit](https://docs.abapgit.org/)

## How-to Guides

- **[Install](pages/how-to-install.md)**
- **[Configure](pages/how-to-configure.md)**
- **[Execute](pages/how-to-execute.md)**
- **[Contribute](pages/how-to-contribute.md)**

## Recommended Readings

- [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md)
- [ABAP 7.40 Quick Reference](https://blogs.sap.com/2015/10/25/abap-740-quick-reference/)
- [ABAP Built-in Functions](https://blogs.sap.com/2015/11/30/reminder-abap-built-in-functions/)
- [ABAP - Release-Specific Changes](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abennews.htm)

## Recommended Courses

- [Writing Testable Code for ABAP](https://open.sap.com/courses/wtc1/items/2gzG0sRlN1yjkTUREB02L9)
- [ABAP Development for S/4HANA](https://open.sap.com/courses/a4h1)

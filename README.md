# code pal for ABAP

[![abap package version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/SAP/code-pal-for-abap/src/y_code_pal_version.intf.abap/abap&label=version)](changelog.txt)
[![license](https://img.shields.io/github/license/SAP/code-pal-for-abap)](LICENSE)
[![REUSE status](https://api.reuse.software/badge/github.com/SAP/code-pal-for-abap)](https://api.reuse.software/info/github.com/SAP/code-pal-for-abap)

Based on the [SAP Code Style Guides](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md), this tool contains a set of checks to guarantee the [SAP Code Style Guides](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md) adherence.  
Together, we both support you in writing a clean ABAP code. 

❣️ It's **free** and **open-source**.

## Features

- Supported by Code Inspector (`SCI` / `SCII`) and ABAP Test Cockpit (ATC) (`SE80` / Eclipse);
- Ignores automatically generated code;
- Supports exemptions;
- Provides a new user-friendly way to create variants ([Profile Management](pages/how-to-configure.md));
- Allows configuration based on:
  - Threshold;
  - Severity / Priority;
  - Validity for productive code, test code, or both;
  - Validity for objects created since a specific date;
  - Validity period;
  - Allow / Deny exemption.
 
⚠️ Tool is not RFC-Enabled ([#268](https://github.com/SAP/code-pal-for-abap/issues/268)). 

⚠️ Severities set to `Notification` / `Priority 3` by default ([#368](https://github.com/SAP/code-pal-for-abap/issues/368)).

## Set of Checks

- [Check Documentation](docs/check_documentation.md)

## Dependencies

- SAP NetWeaver **7.40 SP8** or higher
- [abapGit](https://docs.abapgit.org/)

## How-to Guides

- **[Install](pages/how-to-install.md)**
- **[Configure](pages/how-to-configure.md)**
- **[Execute](pages/how-to-execute.md)**
- **[Contribute](pages/how-to-contribute.md)**

## Recommended Readings

- [SAP Code Style Guides](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md)
- [ABAP 7.40 Quick Reference](https://blogs.sap.com/2015/10/25/abap-740-quick-reference/)
- [ABAP Built-in Functions](https://blogs.sap.com/2015/11/30/reminder-abap-built-in-functions/)
- [ABAP - Release-Specific Changes](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abennews.htm)

## Recommended Courses

- [Writing Testable Code for ABAP](https://open.sap.com/courses/wtc1/items/2gzG0sRlN1yjkTUREB02L9)
- [ABAP Development for S/4HANA](https://open.sap.com/courses/a4h1)

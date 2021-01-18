# code pal for ABAP

[![abap package version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/SAP/code-pal-for-abap/src/y_code_pal_version.intf.abap/abap&label=version)](https://github.com/SAP/code-pal-for-abap/blob/master/changelog.txt)
[![license](https://img.shields.io/github/license/SAP/code-pal-for-abap)](LICENSE)

This tool supports you in writing a clean ABAP code. Clean Code allows you to read your code like well-written prose, it is easily understandable, maintainable, and extensible. In addition, you can write high quality and reliable Unit Tests without hurdles and thereby reduce the total cost of ownership of the software.

Furthermore... It's **free** and **open-source**!

It is licensed under the Apache License, Version 2.0 - see [LICENSE](LICENSE).

## News

**:warning: Migration Required :warning:**

From version `v1.01.0` to `v1.02.0` the folder logic was changed, and the abapGit is not able to perform this migration automatically. Therefore, please, follow the step-by-step described in [How To Migrate](pages/how-to-migrate.md)!

Check the [change log](changelog.txt) for further information.

## Features

- Checks are ready-to-use;
- Checks can be executed via SUT, SCI, SCII (Code-Inspector) and via ATC (e.g. directly in SE80, Eclipse...);
- Automatic exclusion of code which shall not be checked (e.g. generated code);
- The scope is customizable (filter option on object creation date);
- Thresholds are customizable;
- Message severity is customizable (error, warning, or notification);
- Checking may be activated in productive-code and/or test-code (customizable);
- Findings can be suppressed via Code-Inspector Pragmas;
- Usage of a "Profile Management Tool" (Optional).

The Profile Management Tool, which is an alternative of working with SCI based variant(s), offers among others:
- An user friendly UI;
- A user-based or group-based execution of Checks;
- Creation, assignment and unassignment of Profiles (unassigning a profile will only delete it if no checks are registered to this profile);
- Import/Export a profile among systems (download/upload function – UI button or API Post Service);
- Single-Click registration of all code pal checks at once to a profile (UI button);
- Multiple execution of several profiles at the same time (the sharpest/strongest and time valid configuration/threshold will be taken);
- Delegation principle to facilitate team work (all delegates, for instance: team members, can work with the same profile);

:warning: The checks are not RFC-Enabled due to local dependencies. 

## List of Checks

- [Check Documentation](docs/check_documentation.md)

## Dependencies

- SAP NetWeaver **7.40 SP8** or higher
- [abapGit](https://docs.abapgit.org/)

## How-to Guides

- **[Install](pages/how-to-install.md)**
- **[Configure](pages/how-to-configure.md)**
- **[Execute](pages/how-to-execute.md)**
- **[Contribute](pages/how-to-contribute.md)**

## Further Reading

- [ABAP Styleguides on Clean Code](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md)
- [Writing Testable Code for ABAP](https://open.sap.com/courses/wtc1/items/2gzG0sRlN1yjkTUREB02L9)
- [ABAP Development for S/4HANA](https://open.sap.com/courses/a4h1)
- [ABAP 7.40 Quick Reference](https://blogs.sap.com/2015/10/25/abap-740-quick-reference/)
- [ABAP Built-in Functions](https://blogs.sap.com/2015/11/30/reminder-abap-built-in-functions/)
- [ABAP - Release-Specific Changes](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abennews.htm)

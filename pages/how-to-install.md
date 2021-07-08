# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Install](how-to-install.md)

## How to Install

### 1. Clone repository using abapGit

Follow the step-by-step available in the abapGit documentation: [Installing online repo](https://docs.abapgit.org/guide-online-install.html).

> Please, use folder logic `PREFIX`.

![how to clone repository](imgs/clone-repository.png)

### 2. Activate code pal for ABAP

Execute the report `Y_CI_CHECK_REGISTRATION` using the run mode `Activate`.

### 3. Create code inspector variant

Start the transaction `SCI` again, and create a new global check variant.  
Then, select the `code pal for ABAP` group and save it.

![how to create code inspector variant](imgs/sci-check-variant.png)

### 4. User Parameter

It requires you to set the ABAP Test Cockpit (ATC) to run in Code Inspector mode.

Start the transaction `SU3`, and add/set the user parameter `SATC_CI_MODE` to `X`:

![user parameter](imgs/user-parameter.png)

### 5. Service

> :warning: Optional Feature!

Start the transaction `SICF`, and create a service using the `Y_CODE_PAL_SERVICE` handler. Use procedure `Standard`, set the security session to `Completely Restricted`, and check the `Use all logon procedures`.

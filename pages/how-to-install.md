# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Install](how-to-install.md)

## How to Install

### 1. Clone repository using abapGit

Follow the step-by-step available in the abapGit documentation: [Installing online repo](https://docs.abapgit.org/guide-online-install.html).

![how to clone repository](imgs/clone-repository.png)

### 2. Activate code pal for ABAP category

Start the transaction `SCI`, and go to the `Code Inspector > Management of > Checks` menu.  
Then, select the `Y_CATEGORY_CODE_PAL` check class and save it.

![how to activate the category](imgs/sci-management-of-checks.png)

(In some system releases, the path to the `Check Management` might differ from the screenshot.)

### 3. Activate code pal for ABAP checks

Start the transaction `SCI` again, and go to the `Code Inspector > Management of > Checks` menu.  
Then, select all the `Y_CHECK_*` check classes and save it.

![how to activate the checks](imgs/sci-checks.png)

### 4. Create code inspector variant

Start the transaction `SCI` again, and create a new Global Check Variant.  
Then, select the `code pal for ABAP` group and save it.

![how to create code inspector variant](imgs/sci-check-variant.png)

### 5. User Parameter

It requires you to set the ABAP Test Cockpit (ATC) to run in Code Inspector mode.

Start the transaction `SU3`, and add/set the user parameter `SATC_CI_MODE` to `X`:

![user parameter](imgs/user-parameter.png)

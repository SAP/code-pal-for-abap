# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Configure](how-to-configure.md)

## How to Configure

**The default customizing is not an SAP delivered standard value.**  

You can:

* Define check's validity period;
* Restrict to objects created since a specific date;
* Define check's severity;
* Define check's threshold;
* Define if it is applicable in productive codes;
* Define if it is applicable in test codes.

Profile behaviors:

* If you unassign a profile from your user, it will not delete it from the database. It means, you can reassign it to your user, and all the checks with respective customization will return;

Check behaviors:

* If a profile is assigned to your user, it will execute the checks assigned to it only;
* If multiple profiles are assigned to your user, it will execute the checks assigned to them only;
* If have no profile assigned to your user, it will execute all the checks assigned to the variant;

Threshold behaviors:

* If you have multiple profiles, and the same check assigned to two or more profiles, it will use the check with the strongest threshold.
  
### 1. Create or Assign a Profile

Start transaction `Y_CODE_PAL_PROFILE`, click on the `+` button, and inform the profile name.

![create a profile](imgs/create-profile.png)

You can assign an already existing profile to your user. It is useful for working on a team based on the same checks.

### 2. Assign Delegates

Delegates are users which can maintain the checks. As you are creating a new check, you will be added automatically. In case you want to add someone else as an owner, click on the `+` button and inform his/her user name.

![assign delegate](imgs/assign-delegate.png)

### 3. Assign Checks

Click on the `+` button and assign the checks.

![assign check](imgs/assign-check.png)

If you want, you can change the default configuration:

![customize check](imgs/customize-check.png)

If you do not understand the check meaning, you can check its documentation:

![check documentation](imgs/check-documentation.png)

## How to export and import customization

You can export and import profiles, with respective delegates and checks, using a `JSON` file.

It is useful when you work with multiple systems, and you want to sync the profiles between them.

![import and export feature](imgs/import-export-feature.png)

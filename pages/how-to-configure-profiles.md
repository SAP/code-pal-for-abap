# Code Pal for ABAP 

## How to Configure via Profiles

**Important**:
* If you have any profile active for your user, the code pal for ABAP will execute only the checks assigned to an active profile;
* If you unassign a profile, the tool won't delete it. It means, you can reassign it to your user, and all the checks with respective customization will come back;
* If you have multiple profiles, and the same check assigned to two or more profiles, the code pal for ABAP will use the check with the strongest threshold.

### 1. Create Profile
Start transaction `Y_CODE_PAL_PROFILE`, click on the `+` button, and inform the profile name.

![](imgs/create-profile.png)

You can assign an already existing profile to your user. It is useful for working on a team based on the same checks.

### 2. Assign Delegates
Delegates are users which can maintain the checks. As you are creating a new check, you will be added automatically. In case you want to add someone else as an owner, click on the `+` button and inform his/her user name. 

![](imgs/assign-delegate.png)

### 3. Assign Checks
Click on the `+` button and assign the checks.

![](imgs/assign-check.png)

The check has a default customizing (this is not an SAP delivered standard value).  
If you want to change the default configuration, you can set it here:

![](imgs/customize-check.png)

The check documentation is available in the help button:

![](imgs/check-documentation.png)

## How to export and import customization 
The export/import feature allows you to export and import a profile with its delegates and checks between systems using a `JSON` file. 

![](imgs/import-export-feature.png)

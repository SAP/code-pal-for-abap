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

Check behaviors:

* If a profile is assigned to your user, it will execute the checks assigned to it only;
* If multiple profiles are assigned to your user, it will execute the checks assigned to them only;
* If have no profile assigned to your user, it will execute all the checks assigned to the variant.

Profile behaviors:

* If you unassign a profile from your user, it will not delete it from the database. It means, you can reassign it to your user, and all the checks with respective customization will return;
* If you assign the `Y_CHECK_PROFILE_MESSAGE` check to the profile, you will receive an info message every time you execute the global check variant. It means, not all the checks from the variant were executed, but so the ones related to the active profile.

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

### Import via API

Once you export a profile to a `JSON` file, you can import it using the service created in the [How To Install](how-to-install.md) guide.

To consume the API, you have to `POST` the `JSON` file to the service with the respective authentication you configured to the service (usually basic, user/pass) and with the header `Content-Type` as `application/json` and `action` as `import_profile`.

The API returns an `HTTP 400 - Bad Request` if the file format is not valid, or if the request has a wrong `Content-Type`.

The API returns an `HTTP 403 - Forbidden` if the profile already exists in the system and the authentication user is not listed as a delegate.

The API returns an `HTTP 500 - Internal Server Error` if the functionality is not working as expected.

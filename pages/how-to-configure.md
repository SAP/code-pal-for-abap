# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Configure](how-to-configure.md)

## How to Configure

💡 The tool is compatible with the SAP Code Inspector framework (`SCI` / `SCII`), and you can find the respective checks under the `code pal for ABAP (open source plugin)` category once you have it installed.

⚠️The below documentation is valid for our own `Profile Management Tool` feature only.

The transaction `Y_CODE_PAL_PROFILE` provides access to the `Profile Management Tool`.
  
### 1. Profile

> You can see a Profile as a Code Inspector Variant.

Behavior:
- If you assign a Profile to your user, it overwrites the Code Inspector Variant;
- If you assign multiple Profiles to your user, the tool will combine them in runtime;
- You can assign someone else Profile to your user;
- The Profile is deleted once it has no check and assigned to nobody.

To create or assign it, click on the `+` button, and inform the Profile name:

![create a profile](imgs/create-profile.png)

### 2. Delegates

Delegates are users which can maintain the checks. As you are creating a new check, you will be added automatically. In case you want to add someone else as an owner, click on the `+` button and inform his/her user name.

![assign delegate](imgs/assign-delegate.png)

### 3. Checks

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

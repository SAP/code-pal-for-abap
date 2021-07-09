# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Configure](how-to-configure.md)

## How to Configure

⚠️This documentation describes how to configure using the `Profile Management Tool` feature only.

💡Alternatively, you can use the SAP Code Inspector variants in the `SCI` transaction.

Table of Contents:

- [How to Configure](#how-to-configure)
  - [Profiles](#1-profiles)
  - [Delegates](#2-delegates)
  - [Checks](#3-checks)
- [Further Features](#further-features)
  - [Import / Export Profile](#import--export-profile)
  - [Import via API](#import-via-api)
  - [Add / Remove All Checks](#add--remove-all-checks)
  - [Add Missing Checks](#add-missing-checks)

💡 The transaction `Y_CODE_PAL_PROFILE` provides access to the `Profile Management Tool`.

### Profiles

> Profiles are similar to Code Inspector Variants.

Behavior:

- (❗) If you assign a Profile to your user, **it overwrites the Code Inspector variant** (❗);
- If you assign multiple Profiles to your user, the tool will combine them in runtime;
- You can assign someone else Profile to your user;
- The Profile is deleted once it has no check and assigned to nobody.

To create or assign it, click on the `+` button, and inform the Profile name:

![create a profile](imgs/create-profile.png)

### Delegates

> Delegates are the Profile owners who are allowed to configure it;
> Multiple delegates are allowed.

Behavior:

- If you aren't a Delegate, you won't be able to add / change / remove a Delegate or Check.

To add someone else, click on the `+` button and inform his / her user name:

![assign delegate](imgs/assign-delegate.png)

### Checks

> Checks are the rules based on the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md/blob/main/clean-abap/CleanABAP.md) style guide.

Behavior:

- You can define a Check threshold (if applicable);  
- You can define a Check severity / priority (error / priority 1, warning / priority 2, or notification / priority 3)
- You can define a Check filter on productive code, test code, or both (if applicable);
- You can define a Check filter on object creation date;
- You can define a Check validity period;
- You can define if a Check can/cannot be exempt via pseudo-comments (if applicable). 
- If you have multiple Profiles assigned to your user, the check with the"strongest" or "sharpest" thresholds will be taken;

To assign them to your Profile, click on the `+` button:

![assign check](imgs/assign-check.png)

![customize check](imgs/customize-check.png)

💡 You can use the documentation button to navigate to the Check documentation:

![check documentation](imgs/check-documentation.png)

## Further Features

### Import / Export Profile

You can import and export a Profile with its Delegates and Checks using a `JSON` file, here:

![import and export feature](imgs/import-export-feature.png)

### Import via API

Once you export a profile to a `JSON` file, you can import it using the service created in the [How To Install](how-to-install.md) guide.

To consume the API, you have to `POST` the `JSON` file to the service with the respective authentication you configured to the service (usually basic, user/pass) and with the headers `Content-Type` as `application/json` and `action` as `import_profile`.

Possible returns:

- `HTTP 400 - Bad Request` if the file format is not valid, or if the request has a wrong `Content-Type`;
- `HTTP 403 - Forbidden` if the profile already exists in the system and the authentication user is not listed as a delegate;
- `HTTP 500 - Internal Server Error` if the functionality is not working as expected.

### Add / Remove All Checks

You can add all and remove all the Checks from a Profile, here:

![add all and remove all](imgs/)

### Add Missing Checks

You can add all the missing checks, comparing your Profile and the available Checks, here:

![missing checks](imgs/)

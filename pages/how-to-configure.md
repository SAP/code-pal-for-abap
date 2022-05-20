# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Configure](how-to-configure.md)

## How to Configure

This documentation describes how to configure the Code Pal checks. You can do this directly in your check variant (transaction `SCI`) or via the Profile Management Tool (transaction `Y_CODE_PAL_PROFILE`).

Table of Contents:

  - [Check Parameters](#check-parameters)
  - [Profile management](#profile-management)
    - [Delegates](#delegates)

## Check parameters

All Code Pal checks share a common set of parameters:

 - "Consider Objects created after" is a date threshold. Objects whose creation date in the checked system is earlier than this will not be checked.
 - "Message Severity" allows you to choose what priority this check's findings should have.
 - "Apply on Test Code" allows you to declare that code in ABAPUnit test classes should not be checked.
 - "Apply on Productive Code" is the inverse setting - all code that is not "test code" is productive.
 - "Allow "# EC_..."" (where "EC_..." differs from check to check) toggles whether or not the check takes pseudo comments into account. This settings is redundant if you are executing the checks via ATC since the ATC has its own setting for how pseudo comments are treated.

Some checks additionally have a "threshold" parameter - if they count some quantity, like the ["Number of Methods" check](../docs/checks/number-methods.md), then the check will only report findings when its counter exceeds this threshold.

In the Profile Management Tool, there is an additional "Validity Since" parameter that determines a time period during which the configuration in the profile will overwrite the setting in any Code Inspector variant executed by assigned users.

## Profile management

Profiles are similar to Code Inspector check variants, but they can be assigned to specific users and will **overwrite** any settings in a Code Inspector variant. It is recommended to either use different check variant configurations for different purposes **or** individual profiles for different users, but not both at the same time.

Main features:

- If you assign a profile to your user, **it overwrites the Code Inspector variant**
- If you assign multiple profiles to your user, the tool will combine them at runtime
- You can assign another user's profile to your user
- A profile is deleted if it has contains no checks and is assigned to no users.

To create or assign a profile, click on the `+` button, and enter a name for the profile.

![An image showing the Profile Management Tool's main screen, with the '+' button in the "Profiles" area highlighted](imgs/create-profile.png)

### Delegates

Delegates are "owners" of a profile; these are the users how are allowed to modify or delete the profile. If you aren't a delegate, you won't be able to add / change / remove a delegate or check. To add someone else as a delegate, click on the `+` button and enter their user name:

![An image showing the Profile Management Tool's main screen, with the '+' button in the "Delegate" area highlighted](imgs/assign-delegate.png)

You can import and export a profile with its delegates and checks in the form of JSON files - for example, in order to transfer a profile between different systems - by clicking the corresponding buttons.

![An image showing the Profile Management Tool's main screen, with the 'Import' and "Export" buttons in the "Profile" area highlighted](imgs/import-export-feature.png)


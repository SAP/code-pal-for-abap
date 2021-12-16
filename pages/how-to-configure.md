# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Configure](how-to-configure.md)

## How to Configure

You can use the Check Variant or the Code Pal Profile. 

💡 If you have at least one Profile assigned to your user, the Check Variant attributes are disabled. 



### 1. Check Variant

It is the native Code Inspector way to configure the Checks. 

In the Check Variant, transaction `SCI`, you can change the Check attributes: 





### 2. Code Pal Profiles

💡 The transaction `Y_CODE_PAL_PROFILE` provides access to the `Profile Management Tool`.

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

> Checks are the rules based on the [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

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

### Add / Remove All Checks

You can add all and remove all the Checks from a Profile, here:

![add all and remove all](imgs/)

### Add Missing Checks

You can add all the missing checks, comparing your Profile and the available Checks, here:

![missing checks](imgs/)

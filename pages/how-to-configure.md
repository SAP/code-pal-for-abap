# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Configure](how-to-configure.md)

## How to Configure

You can use the **Check Variant** or the **Code Pal Profile**. 

💡 If you have at least one Profile assigned to your user, the Check Variant configuration is disabled. 

### 1. Check Variant

> Transaction: `SCI`

It is the Code Inspector native feature to configure the rules.  

In the Check Variant, you can change the Check attributes:  
<TODO: Image>

### 2. Code Pal Profiles

> Transaction: `Y_CODE_PAL_PROFILE`

It is the Code Pal feature to configure the rules.

### Profiles

- **It overwrites any Check Variant**;
- It is user-based;
- Multiple users can be assigned to a Profile;
- Multiple Profiles can be assigned to a user;
- It is deleted if no Check and User is assigned.

To create a new Profile, or assign an already existing Profile to your user, click on the `+` button:

![create a profile](imgs/create-profile.png)

### Checks

- It is the rule itself;
- You can configure each rule based on your needs;
- It identifies the most strict Checks if multiple Profiles are in use;

To assign a Check, click on the `+` button:

![assign check](imgs/assign-check.png)

Available configurations:
- Its validity;
- Its threshold;
- Its severity/priority;
- Its relevance for productive and test codes;
- Its relevance for objects created since a specific date;
- Its relevance for new child objects;
- Its relevance for exemption;

![customize check](imgs/customize-check.png)

### Delegates

- It is the **owner/admin** of the Profile which has the authorization to maintain it;
- It supports multiple users.

To assign a user, click on the `+` button:

![assign delegate](imgs/assign-delegate.png)

## Remote Checks

* Code Pal is required in the Remote Check System only;
* Profiles are allowed, but you have to assign them to the RFC user.

You can use the **Remote Code Inspector** or the **Remote ATC**. 

### 1. Remote Code Inspector

Having the `Reference Check System` set under `SCI` > `Code Inspector` > `Management Of` > `Reference Check System`, you can create a Check Variant using the `In Reference Check System` option.

In the Remote Check System, you can create and configure the Check Variant and Code Pal Profile, as mentioned before.

### 2. Remote ATC

- [Remote Code Analysis in ATC](https://blogs.sap.com/2016/12/12/remote-code-analysis-in-atc-one-central-check-system-for-multiple-systems-on-various-releases/)
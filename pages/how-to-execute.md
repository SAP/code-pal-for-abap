# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Execute](how-to-execute.md)

## How to Execute

You can to execute the "code pal for ABAP checks" against your objects in several different ways. If needed, you can easily simulate an execution of the checks using the provided `Y_DEMO_FAILURES` class (delivered along with the toolkit). The most common ways of executing code pal are by means of:

1. `Code Inspector` (transactions SCI and/or SCII); for more details, refer to the specific sections below.
2. `ABAP Test Cockpit (ATC)` in CI-mode by informing a check variant**. (in most of the ATC transactions/interfaces as usual; e.g.: SE80 --> right mouse-click --> Check --> ABAP Test Cockpit (ATC) with...); hereby is ADT (Eclipse) also supported.
3. `SUT - ABAP CHECK REPORT` running with in a CI-mode (flavour) and also informing a check variant**;  

** It is also possible to execute code pal with a profile (instead of a variant). For that, please refer to transaction: Y_CODE_PAL_PROFILE.

## Executing via Code Inspector (CI)

Start the transaction `SCI` or `SCII`, inform the object selection as well as a global check variant (for instance, you could provide the global variant `Y_CODE_PAL` which is a preset variant containing all existing checks and delivered along with the toolkit).

![code inspector execution](imgs/sap-gui-code-inspector.png)



## Executing via ABAP Test Cockpit (ATC)

![sap gui execution](imgs/execute-sap-gui.png)

In the tab `Options`, you have to inform your global check variant:  

![sap gui atc execution](imgs/sap-gui-atc.png)


#### REMARK: In order to be able to execute the checks directly via ATC, a User-Parameter has to be set as follows: Go to Menu: SYSTEM --> User Profile --> User Data --> then in the tab `Parameters`, make sure this entry is set: `SATC_CI_MODE = X` (refer to screenshot below):  

![image](https://user-images.githubusercontent.com/63100656/113266529-acd10180-92d5-11eb-80d9-c2717f0be1ca.png)




## Executing via Eclipse (ADT)

Select `Run As` > `ABAP Test Cockpit With...` and inform a global check variant (for instance, you could provide the global variant `Y_CODE_PAL` which is a preset variant containing all existing checks and delivered along with the toolkit).

![eclipse execution](imgs/execute-eclipse.png)




## Executing via SUT (ABAP CHECK REPORT)

Start the transaction: `SUT` and call the program: `ABAP CHECK REPORT`. Then, inform the object selection as well as a global check variant (for instance, you could provide the global variant `Y_CODE_PAL` which is a preset variant containing all existing checks and delivered along with the toolkit). Do not forget to select the `ATC Checks`checkbox as well as the `ATC Mode/Flavour` to Code Inspector (refer to screenshot below):


![image](https://user-images.githubusercontent.com/63100656/113267616-da6a7a80-92d6-11eb-803f-f6392d032c79.png)


In summary, it is possible to use our tool directly in SCI/SCII, via ATC Integration, via API Call, via SUT, via CI-Variant or via Profile(s). But keep in mind, one has to choose between using CI-Variants (Code Inspector Variants) or Using Profile/s (via Transaction: Y_CODE_PAL_PROFILE, delivered along with the toolkit). Both features cannot be used in parallel in the same system for the same user. 


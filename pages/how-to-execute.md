# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Execute](how-to-execute.md)

## How to Execute

You can simulate the checks using the provided `Y_DEMO_FAILURES` class (delivered along with the tookit).

### SAP GUI

It is possible to execute the "code pal for ABAP checks" against your objects via:

1. `Code Inspector` (transactions SCI and SCII);
2. `ABAP Test Cockpit (ATC)` in CI-mode by informing a check variant. (in most of the ATC transactions/interfaces as usual; e.g.: SE80 --> right mouse-click --> Check --> ATC with...);
3. `SUT - ABAP CHECK REPORT` running with in a CI-mode (flavour) and also informing a check variant;  

#### ABAP Test Cockpit (ATC)

![sap gui execution](imgs/execute-sap-gui.png)

In the tab `Options`, you have to inform your global check variant:  

![sap gui atc execution](imgs/sap-gui-atc.png)

#### Code Inspector

Start the transaction `SCII`, inform the object selection, and inform your global check variant.

![code inspector execution](imgs/sap-gui-code-inspector.png)

### Eclipse

Select `Run As` > `ABAP Test Cockpit With...`, and inform your global check variant.

![eclipse execution](imgs/execute-eclipse.png)

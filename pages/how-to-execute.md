# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Execute](how-to-execute.md)

You can simulate the checks using the provided `Y_DEMO_FAILURES` class.

## SAP GUI

You can execute via `Code Inspector` and `ABAP Test Cockpit (ATC)` in most of the transactions as usual. However, you have to inform your global check variant.

### ABAP Test Cockpit (ATC)

![sap gui execution](imgs/execute-sap-gui.png)

In the tab `Options`, you have to inform your global check variant:  

![sap gui atc execution](imgs/sap-gui-atc.png)

### Code Inspector

Start the transaction `SCII`, inform the object selection, and inform your global check variant.

![code inspector execution](imgs/sap-gui-code-inspector.png)

## Eclipse

Select `Run As` > `ABAP Test Cockpit With...`, and inform your global check variant.

![eclipse execution](imgs/execute-eclipse.png)

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
INTERFACE local_interface_one.
  CONSTANTS const_name TYPE abap_bool VALUE abap_false.
ENDINTERFACE.

INTERFACE local_interface_two.
ENDINTERFACE.

INTERFACE local_interface_three.
ENDINTERFACE.

INTERFACE local_interface_four.
ENDINTERFACE.

CLASS local_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES local_interface_one.
    INTERFACES local_interface_two.
    INTERFACES local_interface_three.
    INTERFACES local_interface_four.
ENDCLASS.

CLASS local_class IMPLEMENTATION.
ENDCLASS.

CLASS ltc_constants_only DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_constants_only IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_constants_interface( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example.' )

      ( 'INTERFACE lcl_interface.' )
      ( ' CONSTANTS: BEGIN OF struct, ' )
      ( '             int TYPE i VALUE 1,' )
      ( '            END OF struct.' )
      ( ' CONSTANTS const_name TYPE abap_bool VALUE abap_false. ' )
      ( 'ENDINTERFACE.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example.' )

      ( 'INTERFACE lcl_interface. ' )
      ( ' METHODS method_name. ' )
      ( ' CONSTANTS const_name TYPE abap_bool VALUE abap_false. ' )
      ( 'ENDINTERFACE.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example.' )

      ( 'INTERFACE lcl_interface. "#EC CONS_INTF' )
      ( ' CONSTANTS: BEGIN OF struct, ' )
      ( '             int TYPE i VALUE 1,' )
      ( '            END OF struct.' )
      ( ' CONSTANTS const_name TYPE abap_bool VALUE abap_false. ' )
      ( 'ENDINTERFACE.' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_empty_interface DEFINITION INHERITING FROM ltc_constants_only FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_empty_interface IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example.' )

      ( 'INTERFACE lcl_interface. ' )
      ( 'ENDINTERFACE.' )
    ).
  ENDMETHOD.


ENDCLASS.

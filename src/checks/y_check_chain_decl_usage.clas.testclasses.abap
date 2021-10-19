CLASS ltc_variables DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_variables IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: ' )
      ( '      string TYPE string, ' )
      ( '      class TYPE REF TO cl_oo_class, ' )
      ( '      mandt LIKE sy-mandt. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( |    DATA(string) = 'value'. | )
      ( |    DATA(class) = NEW cl_oo_class( 'cl_oo_class' ). | )
      ( '    DATA(mandt) = sy-mandt. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: "#EC CHAIN_DECL_USAG ' )
      ( '      string TYPE string, ' )
      ( '      class TYPE REF TO cl_oo_class, ' )
      ( '      mandt LIKE sy-mandt. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_structures DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_structures IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: BEGIN OF my_first_structure, ' )
      ( '            field_a TYPE i, ' )
      ( '            field_b TYPE i, ' )
      ( '          END OF my_first_structure, ' )
      ( '          BEGIN OF my_second_structure, ' )
      ( '            field_a TYPE i, ' )
      ( '            field_b TYPE i, ' )
      ( '          END OF my_second_structure. ' )
    ).


  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: BEGIN OF my_first_structure, ' )
      ( '            field_a TYPE i, ' )
      ( '            field_b TYPE i, ' )
      ( '          END OF my_first_structure. ' )
      ( '    DATA: BEGIN OF my_second_structure, ' )
      ( '            field_a TYPE i, ' )
      ( '            field_b TYPE i, ' )
      ( '          END OF my_second_structure. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: BEGIN OF my_first_structure, "#EC CHAIN_DECL_USAG ' )
      ( '            field_a TYPE i, ' )
      ( '            field_b TYPE i, ' )
      ( '          END OF my_first_structure, ' )
      ( '          BEGIN OF my_second_structure, ' )
      ( '            field_a TYPE i, ' )
      ( '            field_b TYPE i, ' )
      ( '          END OF my_second_structure. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_constants DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_constants IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    CONSTANTS: ' )
      ( |      string VALUE '1234', | )
      ( '      integer VALUE 1234. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( |    CONSTANTS string VALUE '1234'. | )
      ( |    CONSTANTS integer VALUE 1234. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    CONSTANTS: "#EC CHAIN_DECL_USAG' )
      ( |      string VALUE '1234', | )
      ( '      integer VALUE 1234. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_instance_attributes DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_instance_attributes IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA: ' )
      ( '       att_a TYPE string,' )
      ( '       att_b TYPE string.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA att_a TYPE string.' )
      ( '     DATA att_b TYPE string.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA: "#EC CHAIN_DECL_USAG ' )
      ( '       att_a TYPE string,' )
      ( '       att_b TYPE string.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_static_attributes DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_static_attributes IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA: ' )
      ( '       att_a TYPE string,' )
      ( '       att_b TYPE string.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA att_a TYPE string.' )
      ( '     CLASS-DATA att_b TYPE string.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA: "#EC CHAIN_DECL_USAG ' )
      ( '       att_a TYPE string,' )
      ( '       att_b TYPE string.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_variables DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_variables IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
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



CLASS ltc_structures DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_structures IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
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



CLASS ltc_constants DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_constants IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
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



CLASS ltc_instance_attributes DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_instance_attributes IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
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



CLASS ltc_static_attributes DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_static_attributes IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
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



CLASS ltc_structure_complex DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_structure_complex IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: variable_a TYPE i, ' )
      ( '          BEGIN OF my_thrid_structure, ' )
      ( '            BEGIN OF stru_a, ' )
      ( '             field_a TYPE i, ' )
      ( '            END OF stru_a, ' )
      ( '            BEGIN OF stru_b, ' )
      ( '             field_b TYPE i, ' )
      ( '            END OF stru_b, ' )
      ( '          END OF my_thrid_structure. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: BEGIN OF my_thrid_structure, ' )
      ( '            BEGIN OF stru_a, ' )
      ( '             field_a TYPE i, ' )
      ( '            END OF stru_a, ' )
      ( '            BEGIN OF stru_b, ' )
      ( '             field_b TYPE i, ' )
      ( '            END OF stru_b, ' )
      ( '          END OF my_thrid_structure. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    DATA: variable_a TYPE i, "#EC CHAIN_DECL_USAG ' )
      ( '          BEGIN OF my_thrid_structure, ' )
      ( '            BEGIN OF stru_a, ' )
      ( '             field_a TYPE i, ' )
      ( '            END OF stru_a, ' )
      ( '            BEGIN OF stru_b, ' )
      ( '             field_b TYPE i, ' )
      ( '            END OF stru_b, ' )
      ( '          END OF my_thrid_structure. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_types DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_types IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     TYPES:' )
      ( '       ty_a TYPE string,' )
      ( '       ty_b TYPE string.' )
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
      ( '     TYPES ty_a TYPE string.' )
      ( '     TYPES ty_b TYPE string.' )
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
      ( '     TYPES: "#EC CHAIN_DECL_USAG' )
      ( '       ty_a TYPE string,' )
      ( '       ty_b TYPE string.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_types_complex DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_types_complex IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    TYPES: ty_a TYPE i, ' )
      ( '           BEGIN OF ty_b, ' )
      ( '             BEGIN OF ty_c, ' )
      ( '              ty_1 TYPE i, ' )
      ( '             END OF ty_c, ' )
      ( '             BEGIN OF ty_d, ' )
      ( '              ty_2 TYPE i, ' )
      ( '             END OF ty_d, ' )
      ( '           END OF ty_b. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    TYPES: BEGIN OF ty_a, ' )
      ( '             BEGIN OF ty_b, ' )
      ( '              ty_1 TYPE i, ' )
      ( '             END OF ty_b, ' )
      ( '             BEGIN OF ty_c, ' )
      ( '              ty_2 TYPE i, ' )
      ( '             END OF ty_c, ' )
      ( '           END OF ty_a. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '    TYPES: ty_a TYPE i, "#EC CHAIN_DECL_USAG' )
      ( '           BEGIN OF ty_b, ' )
      ( '             BEGIN OF ty_c, ' )
      ( '              ty_1 TYPE i, ' )
      ( '             END OF ty_c, ' )
      ( '             BEGIN OF ty_d, ' )
      ( '              ty_2 TYPE i, ' )
      ( '             END OF ty_d, ' )
      ( '           END OF ty_b. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_instance_methods DEFINITION INHERITING FROM ltc_variables FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_instance_methods IMPLEMENTATION.


  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS: ' )
      ( '       meth_a,' )
      ( '       meth_b.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD meth_a. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth_b. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_static_methods DEFINITION INHERITING FROM ltc_variables FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_static_methods IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS: ' )
      ( '       meth_a,' )
      ( '       meth_b.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD meth_a. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth_b. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

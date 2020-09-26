CLASS ltc_single_parameter DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_single_parameter IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_omit_optional_exp( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example IMPORTING param1 TYPE string ' )
      ( '                     RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( |     DATA(val1) = example( EXPORTING param1 = 'example' ). | )
      ( '   ENDMETHOD. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example IMPORTING param1 TYPE string ' )
      ( '                     RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS no_issue IMPORTING param1 TYPE string ' )
      ( '                      EXPORTING param2 TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( |     DATA(val1) = example( 'example' ). | )

      ( |     no_issue( EXPORTING param1 = 'example'     | )
      ( |               IMPORTING param2 = DATA(val2) ). | )
      ( '   ENDMETHOD. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD no_issue. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example IMPORTING param1 TYPE string ' )
      ( '                     RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( |     DATA(val1) = example( EXPORTING param1 = 'example' ). "#EC OPTL_EXP | )
      ( '   ENDMETHOD. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_multiple_parameters DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_parameters IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_omit_optional_exp( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example IMPORTING param1 TYPE string ' )
      ( '                               param2 TYPE string ' )
      ( '                     RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( |     DATA(val1) = example( EXPORTING param1 = 'example'    | )
      ( |                                     param2 = 'example' ). | )
      ( '   ENDMETHOD. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example IMPORTING param1 TYPE string ' )
      ( '                               param2 TYPE string ' )
      ( '                     RETURNING VALUE(result) TYPE string. ' )
      ( '     METHODS no_issue EXPORTING param1 TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( |     DATA(val1) = example( param1 = 'example'    | )
      ( |                           param2 = 'example' ). | )
      ( |     no_issue( IMPORTING param1 = val1 ). | )
      ( '   ENDMETHOD. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD no_issue. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example IMPORTING param1 TYPE string ' )
      ( '                               param2 TYPE string ' )
      ( '                     RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( |     DATA(val1) = example( EXPORTING param1 = 'example'    | )
      ( |                                     param2 = 'example' ). "#EC OPTL_EXP | )
      ( '   ENDMETHOD. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_exceptions DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_exceptions IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_omit_optional_exp( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '     METHODS set_number IMPORTING number TYPE i ' )
      ( '                        EXCEPTIONS failed. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     set_number( EXPORTING number = 1 ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD set_number. ' )
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
      ( '     METHODS set_number IMPORTING number TYPE i ' )
      ( '                        EXCEPTIONS failed. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     set_number( EXPORTING number = 1 EXCEPTIONS failed = 2 ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD set_number. ' )
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
      ( '     METHODS set_number IMPORTING number TYPE i ' )
      ( '                        EXCEPTIONS failed. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     set_number( EXPORTING number = 1 ). "#EC OPTL_EXP ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD set_number. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_receiving DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_receiving IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_omit_optional_exp( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '     METHODS get_number IMPORTING position TYPE i ' )
      ( '                        RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     DATA(number) = get_number( EXPORTING position = 10 ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD get_number. ' )
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
      ( '     METHODS get_number IMPORTING position TYPE i ' )
      ( '                        RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     get_number( EXPORTING position = 10 RECEIVING result = DATA(number) ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD get_number. ' )
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
      ( '     METHODS get_number IMPORTING position TYPE i ' )
      ( '                        RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     DATA(number) = get_number( EXPORTING position = 10 ). "#EC OPTL_EXP ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD get_number. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_single_exporting DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_single_exporting IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_omit_optional_exp( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '     METHODS set_number IMPORTING number TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     set_number( EXPORTING number = 1 ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD set_number. ' )
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
      ( '     METHODS set_number IMPORTING number TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     set_number( number = 1 ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD set_number. ' )
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
      ( '     METHODS set_number IMPORTING number TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     set_number( EXPORTING number = 1 ). "#EC OPTL_EXP ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD set_number. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_multiple_exportings DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_exportings IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_omit_optional_exp( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '     METHODS run IMPORTING param1 TYPE i ' )
      ( '                           param2 TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     run( EXPORTING param1 = 1 ' )
      ( '                    param2 = 2 ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD run. ' )
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
      ( '     METHODS run IMPORTING param1 TYPE i ' )
      ( '                           param2 TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     run( param1 = 1 ' )
      ( '          param2 = 2 ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD run. ' )
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
      ( '     METHODS run IMPORTING param1 TYPE i ' )
      ( '                           param2 TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     run( EXPORTING param1 = 1 ' )
      ( '                    param2 = 2 ). "#EC OPTL_EXP ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD run. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_importing DEFINITION INHERITING FROM ltc_single_exporting FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_importing IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '     METHODS run IMPORTING number TYPE i ' )
      ( '                 EXPORTING id TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     run( EXPORTING number = 1 IMPORTING id = DATA(id) ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD run. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_changing DEFINITION INHERITING FROM ltc_single_exporting FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_changing IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '     METHODS get_number IMPORTING position TYPE i ' )
      ( '                        CHANGING number TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )
      ( '     DATA(number) = 5. ' )
      ( '     get_number( EXPORTING position = 10 CHANGING number = number ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD get_number. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

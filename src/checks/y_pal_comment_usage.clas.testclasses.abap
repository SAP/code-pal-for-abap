CLASS ltc_report DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_report IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_comment_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'AT SELECTION-SCREEN.' )
      ( '  DATA name3 TYPE string. ' )
      ( '*COMMENT ' )
      ( '  "do something ' )
      ( '  "do more ' )
      ( '  "do much more ' )
      ( '  "but not any time soon ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '***********************' )
      ( '* INCLUDE ABC' )
      ( '***********************' )
      ( 'AT SELECTION-SCREEN.' )
      ( '  DATA name3 TYPE string. ' )
      ( '  "?<html> ' )
      ( '  "! docu ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    RETURN.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_class DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_class IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )

      ( '  CLASS y_example DEFINITION.' )
      ( '    PUBLIC SECTION. ' )
      ( '      METHODS example.' )
      ( '  ENDCLASS. ' )

      ( '  CLASS y_example IMPLEMENTATION.' )
      ( '    METHOD example.' )
      ( '* COMMENT ' )
      ( '     "do something ' )
      ( '     "do more ' )
      ( '     "do much more ' )
      ( '     "but not any time soon ' )
      ( '    ENDMETHOD.' )
      ( '  ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )

      ( '  CLASS y_example DEFINITION.' )
      ( '    PUBLIC SECTION. ' )
      ( '      METHODS example.' )
      ( '  ENDCLASS. ' )

      ( '  CLASS y_example IMPLEMENTATION.' )
      ( '    METHOD example.' )
      ( '      DATA name3 TYPE string. ' )
      ( '      "?<html> ' )
      ( '      "! docu ' )
      ( '    ENDMETHOD.' )
      ( '  ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_contains_only_asterisk DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_contains_only_asterisk IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '  FORM example.' )
      ( '******* DATA(xyz) = 1.' )
      ( '  ENDFORM.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '***********************' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_include DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_include IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '* INCLUDE ZxxxUO01' )
      ( '*&---------------------------------------------------------------------*' )
      ( '*& Include ZxxxTOP' )
      ( '*&---------------------------------------------------------------------*' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_standard_commented_include DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_standard_commented_include IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '* INCLUDE Zxxx...                         " Local class definition' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_standard_class_comment DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_standard_class_comment IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '*"* use this source file for any type of declarations (class' )
      ( '*"* definitions, interfaces or type declarations) you need for' )
      ( '*"* components in the private section' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_function_module_interface DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_function_module_interface IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '*"----------------------------------------------------------------------' )
      ( '*"*"Local Interface:' )
      ( '*"  IMPORTING' )
      ( '*"     VALUE(I_INDEX) TYPE  N DEFAULT 0' )
      ( '*"  CHANGING' )
      ( '*"     REFERENCE(C_KOMP) TYPE  KOMP' )
      ( '*"     REFERENCE(C_KOMK) TYPE  KOMK' )
      ( '*"----------------------------------------------------------------------' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_pragma DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_pragma IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '  FORM example.' )
      ( |    CONSTANTS mc_last_counter_values_synch TYPE string VALUE 'LAST_VALUES_SYNCH' ##NO_TEXT. | )
      ( |    CONSTANTS pragma_identifier VALUE '##'. | )
      ( '  ENDFORM.' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_pseudo_comment DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_pseudo_comment IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '  FORM example.' )
      ( |    "# This is only one comment | )
      ( '  ENDFORM.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '  FORM example.' )
      ( |    CONSTANTS abc VALUE 'ABCDEFGH'. "#EC STRING_OK | )
      ( '  ENDFORM.' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_module DEFINITION INHERITING FROM ltc_report FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_module IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION.' )
      ( '*&---------------------------------------------------------------------*' )
      ( '*& Module FIELD_STATUS OUTPUT' )
      ( '*&---------------------------------------------------------------------*' )
      ( '*&' )
      ( '*&---------------------------------------------------------------------*' )
    ).
  ENDMETHOD.

ENDCLASS.

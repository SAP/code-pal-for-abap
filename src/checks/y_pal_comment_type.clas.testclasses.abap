CLASS ltc_asterisk DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_asterisk IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_comment_type( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '*    anti-pattern ' )
      ( '     DATA(variable) = 1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '     " Example Comment ' )
      ( '     DATA(variable) = 1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_generated_include DEFINITION INHERITING FROM ltc_asterisk FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_generated_include IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '*&---------------------------------------------------------------------* ' )
      ( '*& Include          y_example                                            ' )
      ( '*&---------------------------------------------------------------------* ' )
    ).
  ENDMETHOD.

ENDCLASS.

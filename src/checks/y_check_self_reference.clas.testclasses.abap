CLASS ltc_method DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_method IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_self_reference( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( '     METHODS meth1. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     me->meth1( ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ). "#EC SELF_REF
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( '     METHODS meth1. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     meth1( ). ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ). "#EC SELF_REF
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( '     METHODS meth1. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     me->meth1( ). "#EC SELF_REF ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ). "#EC SELF_REF
  ENDMETHOD.

ENDCLASS.


CLASS ltc_attribute DEFINITION INHERITING FROM ltc_method FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_attribute IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS constructor IMPORTING name TYPE string. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA name TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD constructor. ' )
      ( '     me->name = name. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

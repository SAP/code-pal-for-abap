CLASS ltc_raise DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_raise IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_non_class_exception( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS example EXCEPTIONS exception. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     RAISE exception. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS cx_demo DEFINITION INHERITING FROM cx_static_check. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS create RETURNING VALUE(result) TYPE REF TO cx_demo. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS cx_demo IMPLEMENTATION. ' )
      ( '   METHOD create. ' )
      ( '     result = NEW cx_demo( ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     EVENTS event. ' )
      ( '     METHODS example RAISING cx_demo. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     RAISE EXCEPTION TYPE cx_demo. ' )
      ( '     RAISE RESUMABLE EXCEPTION TYPE cx_demo. ' )
      ( '     RAISE EVENT event. ' )
      ( '     RAISE EXCEPTION cx_demo=>create( ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS example EXCEPTIONS exception. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     RAISE exception. "#EC NON_CL_EXCEPT' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_message DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_message IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_non_class_exception( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example EXCEPTIONS exception. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     MESSAGE 'test' TYPE 'I' RAISING exception.| )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS cx_demo DEFINITION INHERITING FROM cx_static_check. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example RAISING cx_demo. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA cflag TYPE abap_bool. ' )
      ( '     DATA(iflag) = COND i( WHEN cflag = abap_true  THEN 1 ' )
      ( '                           WHEN cflag = abap_false THEN 0 ' )
      ( |                           ELSE THROW cx_demo_exception( ) ). | )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example EXCEPTIONS exception. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     MESSAGE 'test' TYPE 'I' RAISING exception. "#EC NON_CL_EXCEPT| )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_single_exception DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_single_exception IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_cx_root_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY.' )
      ( '       RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '     CATCH cx_root.' )
      ( '       result = abap_true.' )
      ( '   ENDTRY.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY. ' )
      ( '       RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '     CATCH cx_failed.' )
      ( '       result = abap_true.' )
      ( '   ENDTRY.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY.' )
      ( '       RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '     CATCH cx_root. "#EC NEED_CX_ROOT' )
      ( '       result = abap_true.' )
      ( '   ENDTRY. ' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_multiple_exceptions DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_exceptions IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_cx_root_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY.' )
      ( '       RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '     CATCH cx_failed cx_root.' )
      ( '       result = abap_true.' )
      ( '   ENDTRY.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY. ' )
      ( '       RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '     CATCH cx_failed cx_demo_constructor.' )
      ( '       result = abap_true.' )
      ( '   ENDTRY.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY.' )
      ( '       RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '     CATCH cx_failed cx_root. "#EC NEED_CX_ROOT' )
      ( '       result = abap_true.' )
      ( '   ENDTRY. ' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.

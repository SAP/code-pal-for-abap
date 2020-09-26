CLASS local_test_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_cx_root_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY.' )
      ( '     RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '   CATCH cx_failed cx_root.' )
      ( '     result = abap_true.' )
      ( '   ENDTRY.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY. ' )
      ( '     RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '   CATCH cx_failed.' )
      ( '     result = abap_true.' )
      ( '   ENDTRY.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION.' )
      ( ' METHOD example.' )
      ( '   TRY.' )
      ( '     RAISE EXCEPTION TYPE cx_demo_constructor.' )
      ( '   CATCH cx_root. "#EC NEED_CX_ROOT' )
      ( '     result = abap_true.' )
      ( '   ENDTRY. ' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.

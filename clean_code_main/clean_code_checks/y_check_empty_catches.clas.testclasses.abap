CLASS ltc_class_based DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_class_based IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_empty_catches( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS class_based. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS classname IMPLEMENTATION. ' )
      ( '   METHOD class_based. ' )
      ( '     TRY. ' )
      ( '     CATCH cx_sy_no_handler. ' )
      ( '*      comment' )
      ( '     ENDTRY. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS class_based. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS classname IMPLEMENTATION. ' )
      ( '   METHOD class_based. ' )
      ( '     TRY. ' )
      ( '     CATCH cx_sy_no_handler. ' )
      ( '       DATA cx TYPE c. ' )
      ( '     ENDTRY. ' )
      ( '   ENDMETHOD. ' )
      ( 'ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '    METHODS class_based. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS classname IMPLEMENTATION. ' )
      ( '   METHOD class_based. ' )
      ( '     TRY. ' )
      ( '     CATCH cx_sy_no_handler. "#EC EMPTY_CATCH ' )
      ( '*      comment ' )
      ( '     ENDTRY. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_system_based DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_system_based IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_empty_catches( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS system_based. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS classname IMPLEMENTATION. ' )
      ( '   METHOD system_based. ' )
      ( '     CATCH SYSTEM-EXCEPTIONS OTHERS = 1. ' )
      ( '     ENDCATCH. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS system_based. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS classname IMPLEMENTATION. ' )
      ( '   METHOD system_based. ' )
      ( '     CATCH SYSTEM-EXCEPTIONS OTHERS = 1. ' )
      ( '       ROLLBACK WORK. ' )
      ( '     ENDCATCH. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '    METHODS system_based. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS classname IMPLEMENTATION. ' )
      ( '   METHOD system_based. ' )
      ( '     CATCH SYSTEM-EXCEPTIONS OTHERS = 1. "#EC EMPTY_CATCH ' )
      ( '     ENDCATCH. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

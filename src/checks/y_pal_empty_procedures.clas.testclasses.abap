CLASS ltc_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_empty_procedures( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '*    comment' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(skip) = abap_true. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example.  ' )
      ( '*    comment' )
      ( '   ENDMETHOD. "#EC EMPTY_PROCEDURE ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_form DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_form IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_empty_procedures( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   FORM example. ' )
      ( '     "comment' )
      ( '     "comment' )
      ( '   ENDFORM. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   FORM example. ' )
      ( '     DATA(skip) = abap_true. ' )
      ( '   ENDFORM. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   FORM example. ' )
      ( '     "comment' )
      ( '     "comment' )
      ( '   ENDFORM. "#EC EMPTY_PROCEDURE ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_module DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_module IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_empty_procedures( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   MODULE example. ' )
      ( '   ENDMODULE. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   MODULE example. ' )
      ( '     DATA(skip) = abap_true. ' )
      ( '   ENDMODULE. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   MODULE example. ' )
      ( '   ENDMODULE. "#EC EMPTY_PROCEDURE' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS local_test_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_declaration_in_if( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_true. ' )
      ( '   IF var1 = abap_true. ' )
      ( '     DATA var2 TYPE c. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA var2 TYPE i. ' )
      ( '   DATA(var1) = abap_true. ' )
      ( '   IF var1 = abap_true. ' )
      ( '     var2 = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_true. ' )
      ( '   IF var1 = abap_true. ' )
      ( '     DATA(var2) = 1. "#EC DECL_IN_IF ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

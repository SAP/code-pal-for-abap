CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_equals_sign_chaining( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.          ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA x TYPE abap_bool. ' )
      ( '   DATA y TYPE abap_bool. ' )
      ( '   DATA z TYPE abap_bool. ' )
      ( '   x = abap_false.        ' )
      ( '   y = abap_false.        ' )
      ( '   z = abap_true.         ' )
      ( '   x = y = z.             ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.                                   ' )
      ( ' START-OF-SELECTION.                               ' )
      ( '   DATA x TYPE abap_bool.                          ' )
      ( '   DATA y TYPE abap_bool.                          ' )
      ( '   DATA z TYPE abap_bool.                          ' )
      ( '   x = abap_false.                                 ' )
      ( '   y = abap_false.                                 ' )
      ( '   z = abap_true.                                  ' )
      ( '   x = xsdbool( y = abap_false AND z = abap_true ).' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.                   ' )
      ( ' START-OF-SELECTION.               ' )
      ( '   DATA x TYPE abap_bool.          ' )
      ( '   DATA y TYPE abap_bool.          ' )
      ( '   DATA z TYPE abap_bool.          ' )
      ( '   x = abap_false.                 ' )
      ( '   y = abap_false.                 ' )
      ( '   z = abap_true.                  ' )
      ( '   x = y = z. "#EC EQUALS_CHAINING ' )
    ).
  ENDMETHOD.

ENDCLASS.

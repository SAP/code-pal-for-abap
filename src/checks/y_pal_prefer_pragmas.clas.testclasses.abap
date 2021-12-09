CLASS ltc_pseudo_comment DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_pseudo_comment IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_pragmas( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA a TYPE string.   "#EC NEEDED ' )
      ( '     DATA b TYPE string. ' )
      ( '     a = b. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA a TYPE string.   ##needed ' )
      ( '     DATA b TYPE string. ' )
      ( '     a = b. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_multiple_pseudo_comments DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_pseudo_comments IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_pragmas( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 2.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     TRY. ' )
      ( '         DATA(div_by_zero) = 1 / 0. ' )
      ( '       CATCH cx_root.               "#EC NEEDED  "#EC NO_HANDLER' )
      ( '     ENDTRY. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     TRY. ' )
      ( '         DATA(div_by_zero) = 1 / 0. ' )
      ( '       CATCH cx_root.               ##NEEDED ##NO_HANDLER' )
      ( '     ENDTRY. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.

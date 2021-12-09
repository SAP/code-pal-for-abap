CLASS ltc_pseudo_comment DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_pseudo_comment IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_pseudo_comment_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '   TRY. ' )
      ( '   CATCH cx_sy_arg_out_of_domain. "#EC EMPTY_CATCH' )
      ( '   ENDTRY. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '   TRY. ' )
      ( '   CATCH cx_sy_arg_out_of_domain. ' )
      ( '   ENDTRY. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_pragma DEFINITION INHERITING FROM ltc_pseudo_comment FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_pragma IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '   TRY. ' )
      ( '   CATCH cx_sy_arg_out_of_domain ##NO_HANDLER. ' )
      ( '   ENDTRY. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_alternative_pseudo_comment DEFINITION INHERITING FROM ltc_pseudo_comment FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_alternative_pseudo_comment IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '   TRY. ' )
      ( '   CATCH cx_sy_arg_out_of_domain. "#EC NO_HANDLER' )
      ( '   ENDTRY. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_multiple_pseudo_comments DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_pseudo_comments IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_pseudo_comment_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '   TRY. ' )
      ( '   CATCH cx_sy_arg_out_of_domain. "#EC EMPTY_CATCH #EC NO_HANDLER' )
      ( '   ENDTRY. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( '  START-OF-SELECTION. ' )
      ( '   TRY. ' )
      ( '   CATCH cx_sy_arg_out_of_domain. "#EC NEEDED ' )
      ( '   ENDTRY. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.

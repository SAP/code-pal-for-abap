CLASS ltc_before_statement DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_before_statement IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_comment_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   " Non-indented comment ' )
      ( '     DATA(count) = 1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( '     METHODS empty_method. ' )
      ( '     METHODS asterisk. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     " Indented comment ' )
      ( '     DATA(count) = 1. ' )
      ( '     " Indented comment ' )
      ( '     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 ##NEEDED. ' )
      ( '     " Indented comment ' )
      ( '     DATA(pseudo) = 1. "#EC SOMETHING ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD empty_method. ' )
      ( '     " Empty ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD asterisk. ' )
      ( '*    Not Relevant ' )
      ( '     DATA(count) = 1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_empty_branch DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_empty_branch IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_comment_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     IF sy-mandt = 100. ' )
      ( '     "TODO: Implement it ' )
      ( '     ENDIF. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     IF sy-mandt = 100. ' )
      ( '       "TODO: Implement it' )
      ( '     ENDIF. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_empty_catch DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_empty_catch IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_comment_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA itab TYPE TABLE OF sy-mandt. ' )
      ( '     TRY. ' )
      ( '         DATA(entry) = itab[ 1 ]. ' )
      ( '       CATCH cx_sy_itab_line_not_found. ' )
      ( '       " meaningful comment ' )
      ( '     ENDTRY.  ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA itab TYPE TABLE OF sy-mandt. ' )
      ( '     TRY. ' )
      ( '         DATA(entry) = itab[ 1 ]. ' )
      ( '       CATCH cx_sy_itab_line_not_found. ' )
      ( '         " meaningful comment ' )
      ( '     ENDTRY.  ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_inline DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_inline IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_comment_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(count) = 1. "TODO: Removing Inline Comment ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     " TODO: Inline Comment ' )
      ( '     DATA(count) = 1. ' )
      ( '     " TODO: Review Pragma ' )
      ( '     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 ##NEEDED. ' )
      ( '     " TODO: Review Pseudo Comment ' )
      ( '     DATA(pseudo) = 1. "#EC SOMETHING ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_pseudo_comment DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_pseudo_comment IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_comment_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA lt_callstack TYPE sys_callst. ' )

      ( '       " Reading stack for program ' )
      ( '     READ TABLE lt_callstack "#EC CI_STDSEQ ' )
      ( |       WITH KEY progname = 'SAPLEDOC_AIF' | )
      ( '       TRANSPORTING NO FIELDS. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA lt_callstack TYPE sys_callst. ' )

      ( '     " Reading stack for program ' )
      ( '     READ TABLE lt_callstack "#EC CI_STDSEQ ' )
      ( |       WITH KEY progname = 'SAPLEDOC_AIF' | )
      ( '       TRANSPORTING NO FIELDS. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #( ).
  ENDMETHOD.

ENDCLASS.

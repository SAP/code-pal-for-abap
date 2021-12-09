CLASS ltc_after_loop DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_after_loop IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '     CHECK sy-mandt = 100.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '     IF sy-mandt = 100.' )
      ( '       RETURN.' )
      ( '     ENDIF.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '     CHECK sy-mandt = 100. "#EC CHECK_POSITION' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_before_loop DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_before_loop IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA(client) = sy-mandt.' )
      ( '     CHECK client = 100.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     CHECK sy-mandt = 100.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA(client) = sy-mandt.' )
      ( '     CHECK client = 100. "#EC CHECK_POSITION' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_in_loop DEFINITION INHERITING FROM ltc_before_loop FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_in_loop IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       CHECK <tadir>-delflag = abap_true.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_inline_data_declaration DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_inline_data_declaration IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(age) = 28. ' )
      ( '     CHECK sy-mandt = 100. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA age TYPE i. ' )
      ( '     CHECK sy-mandt = 100. ' )
      ( '     age = 28. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(age) = 28. ' )
      ( '     CHECK sy-mandt = 100. "#EC CHECK_POSITION ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_field_symbol DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_field_symbol IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     FIELD-SYMBOLS <fs> LIKE sy-mandt. ' )
      ( '     <fs> = sy-mandt. ' )
      ( '     CHECK <fs> = 100. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     FIELD-SYMBOLS <fs> LIKE sy-mandt. ' )
      ( '     CHECK sy-mandt = 100. ' )
      ( '     <fs> = sy-mandt. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     FIELD-SYMBOLS <fs> LIKE sy-mandt. ' )
      ( '     <fs> = sy-mandt. ' )
      ( '     CHECK <fs> = 100. "#EC CHECK_POSITION ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_types DEFINITION INHERITING FROM ltc_inline_data_declaration FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_types IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     TYPES: BEGIN OF line, ' )
      ( '        col1 TYPE string, ' )
      ( '        col2 TYPE string, ' )
      ( '      END OF line. ' )
      ( '     CHECK sy-mandt = 100. ' )
      ( '   ENDMETHOD. ' )
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
    result ?= NEW y_pal_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' PERFORM test. ' )

      ( ' FORM test. ' )
      ( '   DATA(age) = 28. ' )
      ( '   CHECK sy-mandt = 100. ' )
      ( ' ENDFORM. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' PERFORM test. ' )

      ( ' FORM test. ' )
      ( '   CHECK sy-mandt = 100. ' )
      ( '   DATA(age) = 28. ' )
      ( ' ENDFORM. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' PERFORM test. ' )

      ( ' FORM test. ' )
      ( '   DATA(age) = 28. ' )
      ( '   CHECK sy-mandt = 100. "#EC CHECK_POSITION ' )
      ( ' ENDFORM. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_constants DEFINITION INHERITING FROM ltc_inline_data_declaration FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_constants IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     CONSTANTS main_company TYPE i VALUE 100. ' )
      ( '     CHECK sy-mandt = main_company. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_comments DEFINITION INHERITING FROM ltc_inline_data_declaration FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_comments IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     " Comment ' )
      ( '*    Before ' )
      ( '     CHECK sy-mandt = 100. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_pseudo_comment DEFINITION INHERITING FROM ltc_inline_data_declaration FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_pseudo_comment IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     me->example( ). "#EC SELF_REF ' )
      ( '     CHECK sy-mandt = 100. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     CHECK sy-mandt = 100. "#EC CHECK_POSITION ' )
      ( '     CHECK sy-mandt = 100. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

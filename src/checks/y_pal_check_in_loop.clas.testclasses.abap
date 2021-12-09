CLASS ltc_first_position DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_first_position IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_check_in_loop( ).
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
      ( '       CHECK <tadir>-delflag = abap_true.' )
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
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_false.' )
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
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       CHECK <tadir>-delflag = abap_true. "#EC CHECK_IN_LOOP' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_not_in_first_position DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_not_in_first_position IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_check_in_loop( ).
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
      ( '       <tadir>-delflag = abap_false.' )
      ( '       CHECK <tadir>-genflag = abap_true.' )
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
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       <tadir>-delflag = abap_false.' )
      ( '       IF <tadir>-genflag = abap_false.' )
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
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       <tadir>-delflag = abap_false.' )
      ( '       CHECK <tadir>-genflag = abap_true. "#EC CHECK_IN_LOOP' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_multiple_checks DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_checks IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_check_in_loop( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 2.
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
      ( '       CHECK <tadir>-delflag = abap_true.' )
      ( '       CHECK <tadir>-genflag = abap_true.' )
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
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_false' )
      ( '       OR <tadir>-genflag = abap_false.' )
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
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       CHECK <tadir>-delflag = abap_true. "#EC CHECK_IN_LOOP' )
      ( '       CHECK <tadir>-genflag = abap_true. "#EC CHECK_IN_LOOP' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_out_loop DEFINITION INHERITING FROM ltc_first_position FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_out_loop IMPLEMENTATION.

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
      ( '       IF <tadir>-delflag = abap_false.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '     CHECK sy-mandt = 100.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

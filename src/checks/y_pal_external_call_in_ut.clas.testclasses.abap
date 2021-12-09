CLASS ltc_submit DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_submit IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW Y_PAL_EXTERNAL_CALL_IN_UT( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS submit FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD submit. ' )
      ( '     SUBMIT demo_program_submit_rep AND RETURN. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     RETURN. ' )
      ( '     "No external call or redirection ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     SUBMIT demo_program_submit_rep AND RETURN.   "#EC EXT_CALL_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_rfc_call_dest DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_rfc_call_dest IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW Y_PAL_EXTERNAL_CALL_IN_UT( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA dest TYPE RFCDEST VALUE 'NONE'. | )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' DESTINATION dest | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum ' )
      ( '       IMPORTING ' )
      ( '         weekday = date. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum ' )
      ( '       IMPORTING ' )
      ( '         weekday = date. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA dest TYPE RFCDEST VALUE 'NONE'. | )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' DESTINATION dest  "#EC EXT_CALL_UT | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum ' )
      ( '       IMPORTING ' )
      ( '         weekday = date. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_rfc_call_task DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_rfc_call_task IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW Y_PAL_EXTERNAL_CALL_IN_UT( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA dest TYPE RFCDEST VALUE 'NONE'. | )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' IN UPDATE TASK | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum ' )
      ( '       IMPORTING ' )
      ( '         weekday = date. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum ' )
      ( '       IMPORTING ' )
      ( '         weekday = date. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA dest TYPE RFCDEST VALUE 'NONE'. | )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' IN UPDATE TASK  "#EC EXT_CALL_UT | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum ' )
      ( '       IMPORTING ' )
      ( '         weekday = date. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_rfc_call_newtask DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_rfc_call_newtask IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW Y_PAL_EXTERNAL_CALL_IN_UT( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA dest TYPE RFCDEST VALUE 'NONE'. | )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' STARTING NEW TASK 'task' DESTINATION dest | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum ' )
      ( '       IMPORTING ' )
      ( '         weekday = date. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA dest TYPE RFCDEST VALUE 'NONE'. | )
      ( '     DATA date TYPE sy-datum. ' )
      ( |     CALL FUNCTION 'DATE_TO_DAY' STARTING NEW TASK 'task' DESTINATION dest  "#EC EXT_CALL_UT | )
      ( '       EXPORTING ' )
      ( '         date    = sy-datum. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_cl_gui_usage DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_cl_gui_usage IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW Y_PAL_EXTERNAL_CALL_IN_UT( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA lo_gui_alv_cont TYPE REF TO cl_gui_alv_grid. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA exp TYPE ui_functions. ' )
      ( '     APPEND cl_gui_alv_tree_simple=>mc_fc_calculate TO exp.' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA lo_gui_alv_cont TYPE REF TO cl_gui_alv_grid.  "#EC EXT_CALL_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

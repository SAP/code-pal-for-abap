CLASS lth_function DEFINITION.
  PROTECTED SECTION.
    DATA cut TYPE REF TO ltd_check_function.

    METHODS given_rfc_enabled.
    METHODS given_rfc_disabled.
    METHODS when_run.
    METHODS then_has_issue.
    METHODS then_no_issue.

ENDCLASS.


CLASS lth_function IMPLEMENTATION.

  METHOD given_rfc_enabled.
    SELECT SINGLE pname FROM tfdir INTO @DATA(function_group) WHERE fmode = 'R'.
    cut->set_ref_scan( y_code_pal_ref_scan_double=>get_from_fuction_group( function_group ) ).
  ENDMETHOD.

  METHOD given_rfc_disabled.
    SELECT SINGLE pname FROM tfdir INTO @DATA(function_group) WHERE fmode = 'X'.
    cut->set_ref_scan( y_code_pal_ref_scan_double=>get_from_fuction_group( function_group ) ).
  ENDMETHOD.

  METHOD when_run.
    cut->run( ).
  ENDMETHOD.

  METHOD then_no_issue.
    cl_abap_unit_assert=>assert_initial( act = cut->get_statistics( )->count-errors
                                         quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_statistics( )->count-warnings
                                         quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_statistics( )->count-notes
                                         quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_statistics( )->count-pseudo_comments
                                         quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD then_has_issue.
    cl_abap_unit_assert=>assert_not_initial( act = cut->get_statistics( )->count-notes
                                             quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_initial( act = cut->get_statistics( )->count-errors
                                         quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_statistics( )->count-warnings
                                         quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_statistics( )->count-pseudo_comments
                                         quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_function DEFINITION FOR TESTING INHERITING FROM lth_function RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS rfc_enabled FOR TESTING.
    METHODS rfc_disabled FOR TESTING.

  PRIVATE SECTION.
    METHODS setup.

ENDCLASS.


CLASS ltc_function IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD rfc_enabled.
    given_rfc_enabled( ).
    when_run( ).
    then_no_issue( ).
  ENDMETHOD.

  METHOD rfc_disabled.
    given_rfc_disabled( ).
    when_run( ).
    then_has_issue( ).
  ENDMETHOD.

ENDCLASS.

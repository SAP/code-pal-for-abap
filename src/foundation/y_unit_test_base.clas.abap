CLASS y_unit_test_base DEFINITION PUBLIC ABSTRACT FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. "#EC NUMBER_METHODS
  PROTECTED SECTION.
    METHODS bound FOR TESTING.
    METHODS with_issue FOR TESTING.
    METHODS without_issue FOR TESTING.
    METHODS with_exmeption FOR TESTING.
    "! Method to get a check class instance.
    "! @parameter result | Instance of the `Y_CHECK_*` class.
    METHODS get_cut ABSTRACT RETURNING VALUE(result) TYPE REF TO y_check_base.
    "! Method to get a code snippet that raises the check.
    "! @parameter result | Table of strings with the code.
    METHODS get_code_with_issue ABSTRACT RETURNING VALUE(result) TYPE y_char255_tab.
    "! Method to get a code snippet that will not raise the check.
    "! @parameter result | Table of strings with the code.
    METHODS get_code_without_issue ABSTRACT RETURNING VALUE(result) TYPE y_char255_tab.
    "! Method to get a code snippet that will not raise the check because it has a pseudo comment.
    "! @parameter result | Table of strings with the code.
    METHODS get_code_with_exemption ABSTRACT RETURNING VALUE(result) TYPE y_char255_tab.
    "! Method to get the count of raises that the code provided in `get_code_with_issue` will do.
    "! @parameter result | Count of expected raises (Default 1).
    METHODS get_expected_count RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_check_base.
    METHODS setup.
    METHODS given_code_with_issue.
    METHODS given_code_without_issue.
    METHODS given_code_with_exemption.
    METHODS when_run.
    METHODS then_issue.
    METHODS then_no_issue.
    METHODS then_exemption.
    METHODS then_no_exemption.
    METHODS get_issue_count RETURNING VALUE(result) TYPE i.
    METHODS has_pseudo_comment RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS y_unit_test_base IMPLEMENTATION.

  METHOD bound.
    cl_abap_unit_assert=>assert_bound( cut ).
  ENDMETHOD.

  METHOD with_issue.
    given_code_with_issue( ).
    when_run( ).
    then_issue( ).
    then_no_exemption( ).
  ENDMETHOD.

  METHOD without_issue.
    given_code_without_issue( ).
    when_run( ).
    then_no_issue( ).
    then_no_exemption( ).
  ENDMETHOD.

  METHOD with_exmeption.
    CHECK has_pseudo_comment( ).
    given_code_with_exemption( ).
    when_run( ).
    then_no_issue( ).
    then_exemption( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 1.
  ENDMETHOD.

  METHOD setup.
    cut ?= get_cut( ).
    cut->object_name = cl_abap_objectdescr=>describe_by_object_ref( cut )->get_relative_name( ).
    cut->object_type = 'CLAS'.
    cut->has_attributes = abap_false.
    cut->attributes_ok = abap_true.
    cut->clean_code_manager = NEW y_clean_code_manager_double( cut ).
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption(  ).
    cut->statistics = NEW y_scan_statistics( ).
  ENDMETHOD.

  METHOD given_code_without_issue.
    cut->ref_scan = y_code_pal_ref_scan_double=>get( get_code_without_issue(  ) ).
  ENDMETHOD.

  METHOD given_code_with_exemption.
    cut->ref_scan = y_code_pal_ref_scan_double=>get( get_code_with_exemption(  ) ).
  ENDMETHOD.

  METHOD given_code_with_issue.
    cut->ref_scan = y_code_pal_ref_scan_double=>get( get_code_with_issue(  ) ).
  ENDMETHOD.

  METHOD when_run.
    cut->run( ).
  ENDMETHOD.

  METHOD then_issue.
    cl_abap_unit_assert=>assert_equals( act = get_issue_count( )
                                        exp = get_expected_count( ) ).
  ENDMETHOD.

  METHOD then_no_issue.
    cl_abap_unit_assert=>assert_initial( get_issue_count( ) ).
  ENDMETHOD.

  METHOD then_exemption.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->count-pseudo_comments
                                        exp = get_expected_count( ) ).
  ENDMETHOD.

  METHOD then_no_exemption.
    cl_abap_unit_assert=>assert_initial( cut->statistics->count-pseudo_comments ).
  ENDMETHOD.

  METHOD get_issue_count.
    result = COND #( WHEN cut->settings-prio = cl_ci_test_root=>c_error   THEN cut->statistics->count-errors
                     WHEN cut->settings-prio = cl_ci_test_root=>c_warning THEN cut->statistics->count-warnings
                     WHEN cut->settings-prio = cl_ci_test_root=>c_note    THEN cut->statistics->count-notes ).
  ENDMETHOD.

  METHOD has_pseudo_comment.
    result = xsdbool( cut->settings-pseudo_comment IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.

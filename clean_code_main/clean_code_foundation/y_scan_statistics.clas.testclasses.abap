CLASS ltc_statistics DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO y_scan_statistics.

    METHODS:
      setup,
      is_bound FOR TESTING,
      check_errors FOR TESTING,
      check_warnings FOR TESTING,
      check_pseudo_comments FOR TESTING,
      increment_pseudo_comments FOR TESTING.
ENDCLASS.

CLASS ltc_statistics IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_scan_statistics( ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD check_errors.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'E'
        pc   = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 0 ).
  ENDMETHOD.

  METHOD check_warnings.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'W'
        pc   = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comments.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'E'
        pc   = 'P' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 1 ).
  ENDMETHOD.

  METHOD increment_pseudo_comments.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'E'
        pc   = 'P' ).
    cut->y_if_scan_statistics~increment_pseudo_comment_cnt( ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 2 ).
  ENDMETHOD.
ENDCLASS.

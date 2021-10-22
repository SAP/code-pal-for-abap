CLASS ltc_statistics DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO y_if_scan_statistics.

    METHODS:
      setup,
      is_bound FOR TESTING,
      check_errors FOR TESTING,
      check_warnings FOR TESTING,
      check_notes FOR TESTING,
      check_pseudo_comments FOR TESTING,
      increment_pseudo_comments FOR TESTING.
ENDCLASS.

CLASS ltc_statistics IMPLEMENTATION.
  METHOD setup.
    cut ?= NEW y_scan_statistics( ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( cut ).
  ENDMETHOD.

  METHOD check_errors.
    cut->collect( kind = y_check_base=>c_error
                  pc   = '' ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-errors
                                        exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-warnings
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-notes
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-pseudo_comments
                                        exp = 0 ).
  ENDMETHOD.

  METHOD check_warnings.
    cut->collect( kind = y_check_base=>c_warning
                  pc   = '' ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-errors
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-warnings
                                        exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-notes
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-pseudo_comments
                                        exp = 0 ).
  ENDMETHOD.

  METHOD check_notes.
    cut->collect( kind = y_check_base=>c_note
                  pc   = '' ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-errors
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-warnings
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-notes
                                        exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-pseudo_comments
                                        exp = 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comments.
    cut->collect( kind = y_check_base=>c_error
                  pc   = 'P' ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-errors
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-warnings
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-notes
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-pseudo_comments
                                        exp = 1 ).
  ENDMETHOD.

  METHOD increment_pseudo_comments.
    cut->collect( kind = y_check_base=>c_error
                  pc   = 'P' ).

    cut->collect( kind = y_check_base=>c_error
                  pc   = 'P' ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-errors
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-warnings
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-notes
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( act = cut->count-pseudo_comments
                                        exp = 2 ).
  ENDMETHOD.

ENDCLASS.

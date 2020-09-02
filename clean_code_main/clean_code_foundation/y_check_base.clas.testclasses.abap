CLASS ltd_ref_scan_manager DEFINITION INHERITING FROM y_ref_scan_manager_double FOR TESTING.
  PUBLIC SECTION.
    METHODS get_report.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.

  METHOD get_report.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' "Something ' )
    ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_check_base_double DEFINITION FOR TESTING INHERITING FROM y_check_base.
  PROTECTED SECTION.
    METHODS: inspect_tokens REDEFINITION.
ENDCLASS.

CLASS ltc_check_base_double IMPLEMENTATION.
  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_check_configuration DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS given_error_threshold_one.
    METHODS given_error_threshold_five.
    METHODS given_warning_threshold_one.
    METHODS given_warning_threshold_five.
    METHODS given_note_threshold_one.
    METHODS given_note_threshold_five.
    METHODS when_four_errors.
    METHODS when_eight_errors.
    METHODS when_zero_errors.
    METHODS then_expect IMPORTING expected TYPE y_if_clean_code_manager=>check_configuration.
    METHODS then_expect_no_result.
    METHODS cleanup.
  PROTECTED SECTION.
    DATA cut TYPE REF TO y_check_base.
  PRIVATE SECTION.
    DATA actual TYPE y_if_clean_code_manager=>check_configuration.
    METHODS setup.
    METHODS is_bound FOR TESTING.
    METHODS error_vs_error FOR TESTING.
    METHODS error_vs_warning FOR TESTING.
    METHODS error_vs_note FOR TESTING.
    METHODS warning_vs_error FOR TESTING.
    METHODS warning_vs_warning FOR TESTING.
    METHODS warning_vs_note FOR TESTING.
    METHODS note_vs_error FOR TESTING.
    METHODS note_vs_warning FOR TESTING.
    METHODS note_vs_note FOR TESTING.
ENDCLASS.

CLASS y_check_base DEFINITION LOCAL FRIENDS ltc_check_configuration.

CLASS ltc_check_configuration IMPLEMENTATION.
  METHOD setup.
    DATA(ref_scan_manager) = NEW ltd_ref_scan_manager( ).
    ref_scan_manager->get_report( ).

    cut = NEW ltc_check_base_double( ).
    cut->ref_scan_manager ?= ref_scan_manager.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( act = cut ).
  ENDMETHOD.

  METHOD given_error_threshold_one.
    cut->check_configurations = VALUE #( BASE cut->check_configurations
                                       ( prio = 'E' threshold = 1 apply_on_testcode = abap_true apply_on_productive_code = abap_true ) ).
  ENDMETHOD.

  METHOD given_error_threshold_five.
    cut->check_configurations = VALUE #( BASE cut->check_configurations
                                       ( prio = 'E' threshold = 5 apply_on_testcode = abap_true apply_on_productive_code = abap_true ) ).
  ENDMETHOD.

  METHOD given_note_threshold_one.
    cut->check_configurations = VALUE #( BASE cut->check_configurations
                                       ( prio = 'N' threshold = 1 apply_on_testcode = abap_true apply_on_productive_code = abap_true ) ).
  ENDMETHOD.

  METHOD given_note_threshold_five.
    cut->check_configurations = VALUE #( BASE cut->check_configurations
                                       ( prio = 'N' threshold = 5 apply_on_testcode = abap_true apply_on_productive_code = abap_true ) ).
  ENDMETHOD.

  METHOD given_warning_threshold_five.
    cut->check_configurations = VALUE #( BASE cut->check_configurations
                                       ( prio = 'W' threshold = 5 apply_on_testcode = abap_true apply_on_productive_code = abap_true ) ).
  ENDMETHOD.

  METHOD given_warning_threshold_one.
    cut->check_configurations = VALUE #( BASE cut->check_configurations
                                       ( prio = 'W' threshold = 1 apply_on_testcode = abap_true apply_on_productive_code = abap_true ) ).
  ENDMETHOD.

  METHOD when_eight_errors.
    actual = cut->detect_check_configuration( error_count = 8
                                              statement = VALUE #( level = 1 ) ).
  ENDMETHOD.

  METHOD when_four_errors.
    actual = cut->detect_check_configuration( error_count = 4
                                              statement = VALUE #( level = 1 ) ).
  ENDMETHOD.

  METHOD when_zero_errors.
    actual = cut->detect_check_configuration( error_count = 0
                                              statement = VALUE #( level = 1 ) ).
  ENDMETHOD.

  METHOD then_expect.
    cl_abap_unit_assert=>assert_equals( act = actual
                                        exp = expected
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD then_expect_no_result.
    cl_abap_unit_assert=>assert_initial( act = actual
                                         quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD cleanup.
    CLEAR cut->check_configurations.
    CLEAR actual.
  ENDMETHOD.

  METHOD error_vs_error.
    given_error_threshold_one( ).
    given_error_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_error_threshold_one( ).
    given_error_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_error_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_five( ).
    given_error_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_error_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_error_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_five( ).
    given_error_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD error_vs_note.
    given_error_threshold_one( ).
    given_note_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_error_threshold_one( ).
    given_note_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_note_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_five( ).
    given_note_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_note_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_note_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_five( ).
    given_note_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 5 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD error_vs_warning.
    given_error_threshold_one( ).
    given_warning_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_error_threshold_one( ).
    given_warning_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_warning_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_five( ).
    given_warning_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_warning_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_one( ).
    given_warning_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_error_threshold_five( ).
    given_warning_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 5 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD note_vs_error.
    given_note_threshold_one( ).
    given_error_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_note_threshold_one( ).
    given_error_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_error_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_five( ).
    given_error_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_error_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_error_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 5 ] ).
    cleanup( ).

    given_note_threshold_five( ).
    given_error_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD note_vs_note.
    given_note_threshold_one( ).
    given_note_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_note_threshold_one( ).
    given_note_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_note_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_five( ).
    given_note_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_note_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_note_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_five( ).
    given_note_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD note_vs_warning.
    given_note_threshold_one( ).
    given_warning_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_note_threshold_one( ).
    given_warning_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_warning_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_five( ).
    given_warning_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_warning_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_note_threshold_one( ).
    given_warning_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 5 ] ).
    cleanup( ).

    given_note_threshold_five( ).
    given_warning_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD warning_vs_error.
    given_warning_threshold_one( ).
    given_error_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_error_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_error_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_five( ).
    given_error_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_error_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_error_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 5 ] ).
    cleanup( ).

    given_warning_threshold_five( ).
    given_error_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'E' threshold = 1 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD warning_vs_note.
    given_warning_threshold_one( ).
    given_note_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_note_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_note_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_five( ).
    given_note_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'N' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_note_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_note_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_five( ).
    given_note_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 5 ] ).
    cleanup( ).
  ENDMETHOD.

  METHOD warning_vs_warning.
    given_warning_threshold_one( ).
    given_warning_threshold_one( ).
    when_zero_errors( ).
    then_expect_no_result( ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_warning_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_warning_threshold_five( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_five( ).
    given_warning_threshold_one( ).
    when_four_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_warning_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_one( ).
    given_warning_threshold_five( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).

    given_warning_threshold_five( ).
    given_warning_threshold_one( ).
    when_eight_errors( ).
    then_expect( cut->check_configurations[ prio = 'W' threshold = 1 ] ).
    cleanup( ).
  ENDMETHOD.

ENDCLASS.

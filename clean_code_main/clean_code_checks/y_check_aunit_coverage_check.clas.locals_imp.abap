CLASS aunit_coverage_chk_aut DEFINITION INHERITING FROM y_check_aunit_coverage_check
  FOR TESTING FINAL RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS:
      new_checker_created FOR TESTING,
      attr_set_and_get_from_buffer FOR TESTING,
      attributes_checked FOR TESTING,
      complexity_value_too_low FOR TESTING,
      yellow_alert_threshold_wrong FOR TESTING,
      red_alert_threshold_wrong FOR TESTING,
      coverage_checker_runned FOR TESTING,
      unit_test_coverage_checked FOR TESTING,
      unit_test_coverage_not_checked FOR TESTING.

  PRIVATE SECTION.
    CONSTANTS:
      complexity_value TYPE string VALUE '123'.
    METHODS:
      setup.
ENDCLASS.

CLASS aunit_coverage_chk_aut IMPLEMENTATION.
  METHOD setup.
    display_popup = abap_false.
  ENDMETHOD.

  METHOD new_checker_created.
    cl_abap_unit_assert=>assert_bound( act = NEW aunit_coverage_chk_aut( )->complexity_checker ).
  ENDMETHOD.


  METHOD attr_set_and_get_from_buffer.
    threshold_for_complexity = complexity_value.
    DATA(attribute_name) = get_attributes( ).
    put_attributes( p_attributes = attribute_name ).
    cl_abap_unit_assert=>assert_equals( act = threshold_for_complexity exp = complexity_value ).
  ENDMETHOD.

  METHOD attributes_checked.
    if_ci_test~query_attributes( p_display = abap_true ).
    cl_abap_unit_assert=>assert_true( act = attributes_ok ).
  ENDMETHOD.

  METHOD complexity_value_too_low.
    threshold_for_complexity = '0'.
    if_ci_test~query_attributes( p_display = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = sy-msgno exp = '015' ).
  ENDMETHOD.

  METHOD yellow_alert_threshold_wrong.
    threshold_for_yellow_alert = '1'.
    if_ci_test~query_attributes( p_display = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = sy-msgno exp = '016' ).
  ENDMETHOD.

  METHOD red_alert_threshold_wrong.
    threshold_for_red_alert = '-1'.
    if_ci_test~query_attributes( p_display = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = sy-msgno exp = '017' ).
  ENDMETHOD.

  METHOD coverage_checker_runned.
    run( ).
  ENDMETHOD.

  METHOD unit_test_coverage_checked.
    check_unit_test_coverage(
      p_sub_obj_name = 'Y_CHECK_AUNIT_COVERAGE_CHECK'
      p_sub_obj_type = 'CLAS'
      p_param_2      = '|TYP:PROG|NAM:' && sy-repid && '|FLG:|NOS:0|MND:0|CYC:5|CY2:' && complexity_value && '|KNV:0|LCO:0|HDI:0|HVO:0|HEF:0' ).
  ENDMETHOD.

  METHOD unit_test_coverage_not_checked.
    check_unit_test_coverage(
      p_sub_obj_name = sy-repid
      p_sub_obj_type = 'CLAS'
      p_param_2      = '|TYP:PROG|NAM:' && sy-repid && '|FLG:|NOS:0|MND:0|CYC:5|CY2:' && '0' && '|KNV:0|LCO:0|HDI:0|HVO:0|HEF:0' ).
  ENDMETHOD.


ENDCLASS.

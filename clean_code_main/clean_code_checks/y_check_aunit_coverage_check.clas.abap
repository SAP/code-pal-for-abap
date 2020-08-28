class Y_CHECK_AUNIT_COVERAGE_CHECK definition
  public
  inheriting from Y_CHECK_BASE
  create public .

public section.

  methods CONSTRUCTOR .

  methods GET_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
protected section.

  data THRESHOLD_FOR_COMPLEXITY type I value 3 ##NO_TEXT.
  data THRESHOLD_FOR_YELLOW_ALERT type I value 60 ##NO_TEXT.
  data THRESHOLD_FOR_RED_ALERT type I value 20 ##NO_TEXT.
  data DISPLAY_POPUP type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data COMPLEXITY_CHECKER type ref to CL_CI_TEST_METRIC_PROC .

  methods CHECK_UNIT_TEST_COVERAGE
    for event MESSAGE of CL_CI_TEST_METRIC_PROC
    importing
      !P_SUB_OBJ_NAME
      !P_SUB_OBJ_TYPE
      !P_PARAM_2 .

  methods INSPECT_TOKENS
    redefinition .
private section.

  methods GET_CYCLOMATIC_COMPLEXITY
    importing
      !P_PARAM_2 type CSEQUENCE
    returning
      value(CYCLOMATIC_COMPLEXITY_VALUE) type STRING .
  methods GET_UNIT_TEST_RESULT
    importing
      !MAIN_OBJECT type SATC_S_R3TR_KEY
    returning
      value(UNIT_TEST_RESULT) type ref to IF_SCV_RESULT .
  methods PROCESS_UNIT_TEST_RESULT
    importing
      !UNIT_TEST_RESULT type ref to IF_SCV_RESULT .
ENDCLASS.



CLASS Y_CHECK_AUNIT_COVERAGE_CHECK IMPLEMENTATION.


  METHOD CHECK_UNIT_TEST_COVERAGE.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Handler for event cl_ci_test_metric_proc->message: The cyclomatic complexity for a
    " source code object has been calculated. If the complexity exceeds the user defined
    " threshold, check the unit test coverage of the source code
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CHECK p_sub_obj_name IS NOT INITIAL.

    IF get_cyclomatic_complexity( p_param_2 ) < threshold_for_complexity.
      inform( p_test = myname p_kind = c_note p_code = 'eas' ). "Source code object has a low complexity. Unit tests are not mandatory.
      RETURN.
    ENDIF.

    "Get main object (FuGr/Class) of the source codes object, that exceeds the complexity
    DATA(sub_object_name) = p_sub_obj_name.
    DATA(main_object) = cl_satc_ac_program_name_svc=>progname_to_tadir( sub_object_name ).

    "If source code and main object are equal, source is an include. Retrieve the main report of the include,
    IF main_object-obj_name = sub_object_name.
      DATA object_name TYPE trobj_name.
      object_name = p_sub_obj_name.
      main_object-obj_name = cl_ci_objectset=>get_program( p_pgmid = 'R3TR' p_objtype = p_sub_obj_type p_objname = object_name ).
    ENDIF.

    DATA(technical_name_of_main_program) = cl_satc_ac_program_name_svc=>tadir_to_progname(
      obj_type = main_object-obj_type
      obj_name = main_object-obj_name ).

    TRY. "Check the main object for unit tests
        cl_aunit_prog_byte_code_svc=>analyse_program(
          EXPORTING
            program_name = technical_name_of_main_program
          IMPORTING
            source_code_has_tests = DATA(source_code_has_unit_tests) ).
      CATCH cx_aunit_prog_exists_not.
        RETURN. "Source code does not exists -> Ignore this source code
      CATCH cx_aunit_prog_compilation.
        source_code_has_unit_tests = abap_false.
    ENDTRY.

    IF source_code_has_unit_tests = abap_false.
      inform( p_test = myname p_kind = c_error p_code = 'gen' ). "Source exceeds the complexity but has no unit tests
      RETURN.
    ENDIF.

    DATA(unit_test_result) = get_unit_test_result( main_object = main_object ).

    process_unit_test_result( unit_test_result = unit_test_result ). "Return the unit test coverage to the SCI framework

  ENDMETHOD.


  METHOD constructor.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Set up the check: Put check in the right folder, provide a name and a version
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    super->constructor( ).
    description    = 'Unit test coverage'(000).  "The name, under which this check will appear in SCI
    category       = 'Y_CHECK_CATEGORY'.         "The folder, where this check will be located in
    version        = '001'.

    settings-pseudo_comment = '"#EC UNITTEST_MISSING' ##NO_TEXT.
    attributes_ok = abap_true.      "Default values are okay for instant usage
    has_documentation = abap_true.  "This check features a documentation that can be viewed in SCI
    settings-documentation = |{ c_docs_path-checks }abap-unit-coverage.md|.
    has_attributes = abap_true.     "This check features parameters, that can be set by the user in SCI

    DATA(all_messages_of_this_check) = VALUE scimessages(
      ( test = myname code = 'gen' kind = c_error text = 'Source code does not have unit test or test can not be generated'(gen) )
      ( test = myname code = 'fal' kind = c_error text = 'Execution of the unit tests failed'(fal) )
      ( test = myname code = 'tlw' kind = c_error text = 'Unit test coverage too low: &1%'(tlw) )
      ( test = myname code = 'low' kind = c_warning text = 'Unit test coverage low: &1%'(low) )
      ( test = myname code = 'iok' kind = c_note text = 'Unit test coverage ok: &1%'(iok) )
      ( test = myname code = 'eas' kind = c_note text = 'Source code object has a low complexity. Unit tests are not mandatory.'(eas) ) ).
    INSERT LINES OF all_messages_of_this_check INTO TABLE scimessages.

    "The SAP standard check class for cyclomatic complexity will raise an event MESSAGE,
    "if the cyclomatic complexity is exceeded for a given object
    complexity_checker = NEW cl_ci_test_metric_proc( ).
    SET HANDLER check_unit_test_coverage FOR complexity_checker.

  ENDMETHOD.


  METHOD GET_ATTRIBUTES.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Write user-defined parameters for this check to a memory object - Will be needed later
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    EXPORT
      complexity = threshold_for_complexity
      yellow_alert = threshold_for_yellow_alert
      red_alert = threshold_for_red_alert
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD GET_CYCLOMATIC_COMPLEXITY.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Parse the result string from the SAP standard check class for procedural metrics
    " and retrieve the value for cyclomatic complexity
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CONSTANTS:
      cyclomatic_complexity_key TYPE string VALUE 'CY2'.

    "Fallback: If complexity could not be retrieved, the source always exceeds the complexity threshold
    cyclomatic_complexity_value = threshold_for_complexity + 1.

    SPLIT p_param_2 AT '|' INTO TABLE DATA(metrics_of_this_source).
    LOOP AT metrics_of_this_source REFERENCE INTO DATA(current_metric).
      SPLIT current_metric->* AT ':' INTO DATA(metric_name) DATA(metric_value).
      CHECK metric_name = cyclomatic_complexity_key.

      "Value for complexity found, check if it is numeric
      IF metric_value CN space.
        DATA data_type TYPE dd01v-datatype.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in  = metric_value
          IMPORTING
            string_out = metric_value
            htype      = data_type.
        IF data_type = 'NUMC'.
          cyclomatic_complexity_value = metric_value.
        ENDIF.
      ENDIF.

      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_unit_test_result.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Carry out the unit tests of the provided main object and return the test result
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Set up the unit test execution for the provided source code object
    DATA(listener) = cl_saunit_gui_service=>create_listener( ).
    DATA(task) = cl_aucv_task=>create(
      EXPORTING
        i_listener          = listener
        i_measure_coverage  = abap_true
        i_max_risk_level    = cl_aunit_customizing=>get_setup( )-client-max_risk_level ).
    DATA(converted_keys) = VALUE cl_aucv_task=>ty_object_directory_elements(
      ( object = main_object-obj_type obj_name = main_object-obj_name ) ).

    task->add_associated_unit_tests( converted_keys ).

    "Execute the unit test
    task->run( if_aunit_task=>c_run_mode-catch_short_dump ).

    "Calculate the statement coverage of the unit test object
    TRY.
        unit_test_result = task->get_coverage_measurement( )->build_program_result( program_name ).
      CATCH cx_scv_execution_error cx_scv_call_error.
        inform( p_test = myname p_kind = c_error p_code = 'fal' ).
    ENDTRY.

  ENDMETHOD.


  METHOD IF_CI_TEST~QUERY_ATTRIBUTES.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Open a popup window where a user can enter check-parameters and validate his input
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    GET REFERENCE OF threshold_for_complexity INTO DATA(complexity).
    GET REFERENCE OF threshold_for_yellow_alert INTO DATA(yellow_alert).
    GET REFERENCE OF threshold_for_red_alert INTO DATA(red_alert).

    DATA(all_parameters) = VALUE sci_atttab(
    ( ref = complexity text = 'Check sources w. complexity > x'(011) )
    ( ref = yellow_alert text = 'Warning, if coverage < y%'(012) )
    ( ref = red_alert text = 'Error, if coverage < z%'(013) ) ).

    DATA:
      message TYPE char72.
    DO.
      IF display_popup = abap_false.
        IF message IS NOT INITIAL.
          RETURN.
        ENDIF.
      ELSEIF cl_ci_query_attributes=>generic(
               p_name       = myname
               p_title      = 'ABAP Unit test coverage'(000)
               p_attributes = all_parameters
               p_message    = message
               p_display    = p_display ) = abap_true.
        RETURN.
      ENDIF.

      IF complexity->* < 1.
        MESSAGE e015 INTO message. "Minimum value for cyclomatic complexity is 1
        CONTINUE.
      ENDIF.

      "Check the provided % values for unit test coverage
      IF NOT yellow_alert->* BETWEEN red_alert->* AND 99 ##NUMBER_OK.
        MESSAGE e016 INTO message. "Warning-value must be between error-value and 99%
        CONTINUE.
      ENDIF.

      IF NOT red_alert->* BETWEEN 0 AND yellow_alert->*.
        MESSAGE e017 INTO message. "Error-value must be between 0% and warning-value
        CONTINUE.
      ENDIF.

      attributes_ok = abap_true.
      EXIT.

    ENDDO.

  ENDMETHOD.


  METHOD inspect_tokens.
    "Don't really know, where this method is good for.
    "The Code Pal expect it to be implemented...
  ENDMETHOD.


  METHOD PROCESS_UNIT_TEST_RESULT.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Return the (line based) test coverage of the unit tests to the SCI/ATC framework
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CHECK unit_test_result IS BOUND.

    DATA(statement_coverage_in_percent) = unit_test_result->get_coverage( ce_scv_coverage_type=>statement )->get_percentage( ).
    statement_coverage_in_percent = round( val = statement_coverage_in_percent dec = 2 ).

    DATA message_code TYPE sci_errc.
    DATA message_kind TYPE sychar01.

    "Compare the statement coverage of the given source code against the user defined thresholds
    IF statement_coverage_in_percent BETWEEN 0 AND threshold_for_red_alert.
      message_code = 'tlw'.
      message_kind = c_error.
    ELSEIF statement_coverage_in_percent BETWEEN threshold_for_red_alert AND threshold_for_yellow_alert.
      message_code = 'low'.
      message_kind = c_warning.
    ELSE.
      message_code = 'iok'.
      message_kind = c_note.
    ENDIF.
    inform(
      p_test = myname
      p_kind = message_kind
      p_code = message_code
      p_param_1 = CONV string( statement_coverage_in_percent ) ).

  ENDMETHOD.


  METHOD PUT_ATTRIBUTES.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Read parameters for the check from the provided memory object
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IMPORT
      complexity = threshold_for_complexity
      yellow_alert  = threshold_for_yellow_alert
      red_alert  = threshold_for_red_alert
      FROM DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD RUN.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Execute the check
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Pass parameters to SAP standard complexity check
    DATA params_for_complexity_checker TYPE xstring.
    EXPORT
      with_cyc         = abap_true
      with_cy2         = abap_true "Calculcate the cyclomatic complexity
      with_nos         = abap_false
      with_knv         = abap_false
      with_lco         = abap_false
      with_mnd         = abap_false
      with_hdi         = abap_false
      with_hvo         = abap_false
      with_hef         = abap_false
      on_dev_obj_level = abap_true    "Check cyclomatic complexity on main object level
      on_class_level   = abap_false
      show_warning     = abap_false
      threshold_nos    = 0
      threshold_mnd    = 0
      threshold_cyc    = 0
      threshold_cy2    = threshold_for_complexity
      threshold_hdi    = 0
      threshold_hvo    = 0
      threshold_hef    = 0
      coarse_procs     = abap_false
    TO DATA BUFFER params_for_complexity_checker.
    complexity_checker->put_attributes( params_for_complexity_checker ).

    "Execute the SAP standard check for cyclomatic complexity
    "The cyclomatic complexity value will be passed to the handler method CHECK_UNIT_TEST_COVERAGE in this class
    complexity_checker->run( ).

  ENDMETHOD.
ENDCLASS.

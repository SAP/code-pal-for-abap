CLASS y_check_base DEFINITION PUBLIC ABSTRACT
  INHERITING FROM cl_ci_test_scan
  CREATE PUBLIC
  GLOBAL FRIENDS y_unit_test_base
                 y_unit_test_coverage.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF c_code,
                 error        TYPE sci_errc VALUE '100',
                 warning      TYPE sci_errc VALUE '101',
                 notification TYPE sci_errc VALUE '102',
               END OF c_code.

    CONSTANTS c_code_not_maintained TYPE sci_errc VALUE '106' ##NO_TEXT.

    CONSTANTS: BEGIN OF c_docs_path,
                 main   TYPE string VALUE 'https://github.com/SAP/code-pal-for-abap/blob/master/docs/' ##NO_TEXT,
                 checks TYPE string VALUE 'https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/' ##NO_TEXT,
               END OF c_docs_path.

    DATA: BEGIN OF settings READ-ONLY,
            pseudo_comment                TYPE sci_pcom,
            disable_on_prodcode_selection TYPE abap_bool,
            disable_on_testcode_selection TYPE abap_bool,
            disable_threshold_selection   TYPE abap_bool,
            object_created_on             TYPE creationdt,
            threshold                     TYPE ycicc_threshold,
            prio                          TYPE ycicc_message_kind,
            apply_on_productive_code      TYPE ycicc_productive_code,
            apply_on_test_code            TYPE ycicc_testcode,
            documentation                 TYPE c LENGTH 1000,
            is_threshold_reversed         TYPE abap_bool,
            ignore_pseudo_comments        TYPE abap_bool,
          END OF settings.

    METHODS constructor.

    METHODS get_attributes REDEFINITION.
    METHODS if_ci_test~display_documentation  REDEFINITION.
    METHODS if_ci_test~query_attributes REDEFINITION.
    METHODS put_attributes  REDEFINITION.
    METHODS run REDEFINITION.

  PROTECTED SECTION.
    CONSTANTS initial_date TYPE datum VALUE '19000101'.

    DATA check_configurations TYPE y_if_clean_code_manager=>check_configurations.
    DATA clean_code_exemption_handler TYPE REF TO y_if_exemption.
    DATA clean_code_manager TYPE REF TO y_if_clean_code_manager.
    DATA statistics TYPE REF TO y_if_scan_statistics.

    "! <p class="shorttext synchronized" lang="en">Relevant Statement Types for Inspection</p>
    "! There are default values set in the Y_CHECK_BASE, and you can reuse the constants available in the 'scan_struc_stmnt_type' structure to enhance or change it.
    DATA relevant_statement_types TYPE HASHED TABLE OF sstruc-stmnt_type WITH UNIQUE KEY table_line.

    "! <p class="shorttext synchronized" lang="en">Relevant Structure Types for Inspection</p>
    "! There are default values set in the Y_CHECK_BASE, and you can reuse the constants available in the 'scan_struc_type' structure to enhance or change it.
    DATA relevant_structure_types TYPE HASHED TABLE OF sstruc-type WITH UNIQUE KEY table_line.

    METHODS execute_check.

    "! <p class="shorttext synchronized" lang="en">Validates the Customizing</p>
    "! @parameter statement | Received from inspect_tokens method.
    "! @parameter error_count | Number of findings found to compare against the threshold.
    "! @parameter result | Configuration structure if the check must be raised
    METHODS detect_check_configuration IMPORTING statement     TYPE sstmnt
                                                 error_count   TYPE int4 DEFAULT 1
                                       RETURNING VALUE(result) TYPE y_if_clean_code_manager=>check_configuration.

    METHODS get_code IMPORTING message_prio  TYPE sychar01
                     RETURNING VALUE(result) TYPE sci_errc.

    "! <p class="shorttext synchronized" lang="en">Inspect Structures</p>
    METHODS inspect_structures.

    "! <p class="shorttext synchronized" lang="en">Inspect Statements of a Structure</p>
    "! @parameter structure | Leading Structure
    METHODS inspect_statements IMPORTING structure TYPE sstruc.

    "! <p class="shorttext synchronized" lang="en">Inspect Tokens of a Statement</p>
    "! @parameter structure | Leading Structure
    "! @parameter index | Leading Index
    "! @parameter statement | Leading Statement
    METHODS inspect_tokens ABSTRACT IMPORTING structure TYPE sstruc
                                              index     TYPE i
                                              statement TYPE sstmnt.

    METHODS raise_error IMPORTING object_type            TYPE trobjtype DEFAULT c_type_include
                                  statement_level        TYPE stmnt_levl
                                  statement_index        TYPE int4
                                  statement_from         TYPE int4
                                  error_counter          TYPE sci_errcnt OPTIONAL
                                  parameter_01           TYPE csequence OPTIONAL
                                  parameter_02           TYPE csequence OPTIONAL
                                  parameter_03           TYPE csequence OPTIONAL
                                  parameter_04           TYPE csequence OPTIONAL
                                  additional_information TYPE xstring OPTIONAL
                                  check_configuration    TYPE y_if_clean_code_manager=>check_configuration. "#EC OPTL_PARAM

    METHODS set_check_message IMPORTING message TYPE itex132.
    METHODS get_class_description RETURNING VALUE(result) TYPE string.

    METHODS condense_tokens IMPORTING statement TYPE sstmnt
                            RETURNING VALUE(result) TYPE string.

    METHODS is_test_code IMPORTING statement TYPE sstmnt
                         RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS is_threshold_stricter IMPORTING previous TYPE int4
                                            current TYPE int4
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_config_stricter IMPORTING previous TYPE y_if_clean_code_manager=>check_configuration
                                         current TYPE y_if_clean_code_manager=>check_configuration
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_app_comp_in_scope IMPORTING level         TYPE stmnt_levl
                                 RETURNING VALUE(result) TYPE abap_bool.

    METHODS switch_bool IMPORTING boolean       TYPE abap_bool
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS handle_ignore_pseudo_comments IMPORTING  check_configuration TYPE y_if_clean_code_manager=>check_configuration.

    METHODS is_running_unit_test RETURNING VALUE(result) TYPE abap_bool.

    METHODS handle_unit_test_statistics IMPORTING statement_index TYPE i
                                                  check_configuration TYPE y_if_clean_code_manager=>check_configuration.

    METHODS is_statement_in_aunit_tab IMPORTING statement TYPE sstmnt
                                      RETURNING VALUE(result) TYPE abap_bool
                                     RAISING cx_sy_itab_line_not_found.

    METHODS get_tadir_keys IMPORTING statement TYPE sstmnt
                           RETURNING VALUE(result) TYPE tadir.

    METHODS raise_generic_message.
    METHODS handle_profiles.

ENDCLASS.



CLASS Y_CHECK_BASE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    clean_code_manager = NEW y_clean_code_manager( ).
    clean_code_exemption_handler = NEW y_exemption_handler( ).

    description = get_class_description(  ).
    category = 'Y_CATEGORY_CODE_PAL'.
    position = y_code_pal_sorter=>get_position( myname ).
    version = '0000'.
    has_documentation = abap_true.

    settings-object_created_on = '20190101'.
    settings-prio = c_note.
    settings-threshold = 5.
    settings-apply_on_productive_code = abap_true.
    settings-apply_on_test_code = abap_true.
    settings-documentation = |{ c_docs_path-main }check_documentation.md|.
    settings-ignore_pseudo_comments = abap_false.

    handle_profiles( ).

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-form )
                                        ( scan_struc_stmnt_type-method )
                                        ( scan_struc_stmnt_type-function )
                                        ( scan_struc_stmnt_type-module ) ).

    relevant_structure_types = VALUE #( ( scan_struc_type-event ) ).

    INSERT VALUE #( test = myname
                    code = c_code_not_maintained
                    kind = cl_ci_test_root=>c_note
                    text = TEXT-106 ) INTO TABLE scimessages[].
  ENDMETHOD.


  METHOD handle_profiles.
    IF y_profile_manager=>create( )->is_in_use( sy-uname ) = abap_true.
      " Hides SCI attributes button
      has_attributes = abap_false.

      TRY.
          " Configuration based on Profiles
          check_configurations = clean_code_manager->read_check_customizing( myname ).
        CATCH ycx_no_check_customizing.
          CLEAR check_configurations.
      ENDTRY.
    ELSE.
      " Displays SCI attributes button
      has_attributes = abap_true.

      " Default configuration
      check_configurations = VALUE #( ( object_creation_date = settings-object_created_on
                                        prio = settings-prio
                                        apply_on_productive_code = settings-apply_on_productive_code
                                        apply_on_testcode = settings-apply_on_test_code
                                        threshold = settings-threshold
                                        ignore_pseudo_comments = settings-ignore_pseudo_comments ) ).
    ENDIF.

    attributes_ok = xsdbool( check_configurations IS NOT INITIAL ).
  ENDMETHOD.


  METHOD detect_check_configuration.
    DATA(tadir_keys) = get_tadir_keys( statement ).

    DATA(creation_date) = clean_code_manager->calculate_obj_creation_date( object_type = tadir_keys-object
                                                                           object_name = tadir_keys-obj_name  ).

    LOOP AT check_configurations ASSIGNING FIELD-SYMBOL(<configuration>)
    WHERE object_creation_date <= creation_date.
      IF settings-is_threshold_reversed  = abap_true
      AND <configuration>-threshold < error_count.
        CONTINUE.
      ENDIF.

      IF settings-is_threshold_reversed = abap_false
      AND <configuration>-threshold > error_count.
        CONTINUE.
      ENDIF.

      DATA(is_test_code) = is_test_code( statement ).

      IF is_test_code = abap_true
      AND <configuration>-apply_on_testcode = abap_false.
        CONTINUE.
      ELSEIF is_test_code = abap_false
      AND <configuration>-apply_on_productive_code = abap_false.
        CONTINUE.
      ENDIF.

      DATA(is_config_stricter) = is_config_stricter( previous = result
                                                     current = <configuration> ).

      IF is_config_stricter = abap_true.
        no_aunit = xsdbool( <configuration>-apply_on_testcode = abap_false ).
        result = <configuration>.
      ENDIF.
    ENDLOOP.

    IF result IS INITIAL.
      RETURN.
    ENDIF.

    DATA(exempt) = clean_code_exemption_handler->is_object_exempted( object_type = tadir_keys-object
                                                                     object_name = tadir_keys-obj_name  ).

    IF exempt = abap_true.
      CLEAR result.
      RETURN.
    ENDIF.

    IF is_app_comp_in_scope( statement-level ) = abap_false.
      CLEAR result.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD execute_check.
    inspect_structures( ).
  ENDMETHOD.


  METHOD inspect_structures.
    DATA(structures) = FILTER #( ref_scan->structures IN relevant_structure_types WHERE type = table_line ).

    LOOP AT structures ASSIGNING FIELD-SYMBOL(<structure>).
      inspect_statements( <structure> ).
    ENDLOOP.

    structures = FILTER #( ref_scan->structures IN relevant_statement_types WHERE stmnt_type = table_line ).

    LOOP AT structures ASSIGNING <structure>.
      inspect_statements( <structure> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_statements.
    DATA(index) = structure-stmnt_from.

    LOOP AT ref_scan->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from
    TO structure-stmnt_to.
      inspect_tokens( index = index
                      structure = structure
                      statement = <statement> ).

      index = index + 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_attributes.
    DATA(check_configuration) = check_configurations[ 1 ].

    EXPORT
      object_creation_date = check_configuration-object_creation_date
      message_severity = check_configuration-prio
      threshold = check_configuration-threshold
      apply_on_productive_code = check_configuration-apply_on_productive_code
      apply_on_testcode = check_configuration-apply_on_testcode
      ignore_pseudo_comments = check_configuration-ignore_pseudo_comments
    TO DATA BUFFER p_attributes.
  ENDMETHOD.


  METHOD get_code.
    CASE message_prio.
      WHEN c_error.
        result = c_code-error.
      WHEN c_warning.
        result = c_code-warning.
      WHEN c_note.
        result = c_code-notification.
      WHEN OTHERS.
        result = c_code_not_maintained.
    ENDCASE.
  ENDMETHOD.


  METHOD if_ci_test~display_documentation.
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = settings-documentation
        window_name            = ' '
        new_window             = 'X'
      EXCEPTIONS
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD if_ci_test~query_attributes.
    DATA message(72) TYPE c.

    DATA(check_configuration) = check_configurations[ 1 ].

    " Build screen fields
    DATA(screen_attributes) = VALUE sci_atttab( ( kind = ''
                                                  ref  = REF #( check_configuration-object_creation_date )
                                                  text = 'Consider Objects created after'(200) )
                                                ( kind = ''
                                                  ref  = REF #( check_configuration-prio )
                                                  text = 'Message Severity'(201) ) ).

    IF settings-disable_threshold_selection = abap_false.
      screen_attributes = VALUE #( BASE screen_attributes
                                 ( kind = ''
                                   ref  = REF #( check_configuration-threshold )
                                   text = 'Threshold'(203) ) ).
    ENDIF.

    IF settings-disable_on_prodcode_selection = abap_false.
      screen_attributes = VALUE #( BASE screen_attributes
                                 ( kind = ''
                                   ref  = REF #( check_configuration-apply_on_productive_code )
                                   text = 'Apply on Productive Code'(204) ) ).
    ENDIF.

    IF settings-disable_on_testcode_selection = abap_false.
      screen_attributes = VALUE #( BASE screen_attributes
                                 ( kind = ''
                                   ref  = REF #( check_configuration-apply_on_testcode )
                                   text = 'Apply on Testcode'(202) ) ).
    ENDIF.

    check_configuration-ignore_pseudo_comments = switch_bool( check_configuration-ignore_pseudo_comments ).

    IF settings-pseudo_comment IS NOT INITIAL.
      screen_attributes = VALUE #( BASE screen_attributes
                                 ( kind = ''
                                   ref  = REF #( check_configuration-ignore_pseudo_comments )
                                   text = |Allow { settings-pseudo_comment }| ) ).
    ENDIF.

    " Call screen and validate fields
    WHILE 1 = 1.
      DATA(break) = cl_ci_query_attributes=>generic( p_name       = name
                                                     p_title      = |{ description }|
                                                     p_attributes = screen_attributes
                                                     p_message    = message
                                                     p_display    = p_display ).
      IF break = abap_true.
        attributes_ok = abap_true.
        RETURN.
      ENDIF.

      IF check_configuration-object_creation_date IS INITIAL.
        check_configuration-object_creation_date = initial_date.
      ENDIF.

      IF check_configuration-apply_on_productive_code = abap_false
      AND check_configuration-apply_on_testcode = abap_false.
        attributes_ok = abap_false.
        message = 'Choose the Type of Code to be checked'(300).
      ELSEIF check_configuration-prio IS INITIAL.
        attributes_ok = abap_false.
        message = 'Choose a Message Severity'(301).
      ELSE.
        attributes_ok = abap_true.
        EXIT.
      ENDIF.
    ENDWHILE.

    check_configuration-ignore_pseudo_comments = switch_bool( check_configuration-ignore_pseudo_comments ).
    check_configurations[ 1 ] = check_configuration.
  ENDMETHOD.


  METHOD put_attributes.
    DATA check_configuration TYPE y_if_clean_code_manager=>check_configuration.

    " Not required when using Profiles
    CHECK has_attributes = abap_true.

    IMPORT
      object_creation_date = check_configuration-object_creation_date
      message_severity = check_configuration-prio
      threshold = check_configuration-threshold
      apply_on_productive_code = check_configuration-apply_on_productive_code
      apply_on_testcode = check_configuration-apply_on_testcode
      ignore_pseudo_comments = check_configuration-ignore_pseudo_comments
    FROM DATA BUFFER p_attributes.

    " Replace default configuration
    check_configurations[ 1 ] = check_configuration.

    attributes_ok = abap_true.
  ENDMETHOD.


  METHOD raise_error.
    " Probably, the Profile is not relevant (threshold, dates, etc)
    CHECK check_configuration IS NOT INITIAL.

    handle_ignore_pseudo_comments( check_configuration ).

    IF is_running_unit_test( ) = abap_true.
      handle_unit_test_statistics( statement_index =  statement_index
                                   check_configuration = check_configuration ).
    ELSE.
      inform( p_sub_obj_type = object_type
              p_sub_obj_name = get_include( p_level = statement_level )
              p_position = statement_index
              p_line = get_line_abs( statement_from )
              p_column = get_column_abs( statement_from )
              p_errcnt = error_counter
              p_kind = check_configuration-prio
              p_test = myname
              p_code = get_code( check_configuration-prio )
              p_param_1 = parameter_01
              p_param_2 = parameter_02
              p_param_3 = parameter_03
              p_param_4 = parameter_04
              p_detail = additional_information ).
    ENDIF.
  ENDMETHOD.


  METHOD run.
    CHECK attributes_ok = abap_true.

    IF ref_scan IS INITIAL.
      " Force ref_scan->aunit_tab
      no_aunit = abap_true.

      IF get( ) = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    " Object not supported?
    IF ref_scan IS INITIAL
    OR ref_scan->subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    execute_check( ).
  ENDMETHOD.


  METHOD raise_generic_message.
    raise_error( statement_level = 1
                 statement_index = 1
                 statement_from  = 1
                 check_configuration = VALUE #( prio = `E` ) ).
  ENDMETHOD.


  METHOD set_check_message.
    y_message_registration=>add_message(
      EXPORTING
        check_name     = myname
        text           = message
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = scimessages ).
  ENDMETHOD.


  METHOD get_class_description.
    SELECT SINGLE descript INTO @result FROM seoclasstx WHERE clsname = @myname.
    IF sy-subrc <> 0.
      result = 'Description Not Available'.
    ENDIF.
  ENDMETHOD.


  METHOD is_config_stricter.
    result = xsdbool( ( previous IS INITIAL )
                   OR ( previous-prio = current-prio AND is_threshold_stricter( current = current-threshold previous = previous-threshold ) = abap_true )
                   OR ( previous-prio <> c_error AND current-prio = c_error )
                   OR ( previous-prio = c_note AND current-prio = c_warning )
                   OR ( previous-ignore_pseudo_comments = abap_false AND current-ignore_pseudo_comments = abap_true ) ).
  ENDMETHOD.


  METHOD is_threshold_stricter.
    result = xsdbool( ( previous >= current AND settings-is_threshold_reversed = abap_false )
                   OR ( previous < current AND settings-is_threshold_reversed = abap_true ) ).
  ENDMETHOD.


  METHOD is_app_comp_in_scope.
    TRY.
        DATA(main_app_comp) = y_code_pal_app_comp=>get( ref_scan->levels[ level = 0 ] ).
        DATA(curr_app_comp) = y_code_pal_app_comp=>get( ref_scan->levels[ level ] ).
        result = xsdbool( main_app_comp = curr_app_comp ).
      CATCH cx_sy_itab_line_not_found
            ycx_entry_not_found.
        result = abap_true.
    ENDTRY.
  ENDMETHOD.


  METHOD switch_bool.
    result = xsdbool( boolean = abap_false ).
  ENDMETHOD.


  METHOD condense_tokens.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE type <> scan_token_type-comment
    AND type <> scan_token_type-pragma.
      result = |{ result }{ <token>-str } |.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_ignore_pseudo_comments.
    DATA(code) = get_code( check_configuration-prio ).

    LOOP AT scimessages ASSIGNING FIELD-SYMBOL(<message>)
    WHERE test = myname
    AND code = code.
      IF check_configuration-ignore_pseudo_comments = abap_true.
        CLEAR <message>-pcom.
      ELSE.
        <message>-pcom = settings-pseudo_comment+5.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_running_unit_test.
    result = xsdbool( statistics IS BOUND ).
  ENDMETHOD.


  METHOD handle_unit_test_statistics.
    DATA(code) = get_code( check_configuration-prio ).

    DATA(pcom_detector) = NEW y_pseudo_comment_detector( )->is_pseudo_comment( ref_scan = ref_scan
                                                                               scimessages = scimessages
                                                                               test = myname
                                                                               code = code
                                                                               suppress = settings-pseudo_comment
                                                                               position = statement_index ).

    statistics->collect( kind = check_configuration-prio
                         pc = pcom_detector ).
  ENDMETHOD.


  METHOD is_test_code.
    TRY.
        result = is_statement_in_aunit_tab( statement ).
      CATCH cx_sy_itab_line_not_found.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD is_statement_in_aunit_tab.
    " Based on: CL_CI_TEST_SCAN->INFORM()
    DATA: BEGIN OF aunit,
        incl_name  TYPE program,
        line_range TYPE RANGE OF i,
      END OF aunit.

    DATA(include) = get_include( p_level = statement-level ).

    TRY.
        " Local Test Class
        aunit = ref_scan->aunit_tab[ incl_name = include ].
      CATCH cx_sy_itab_line_not_found.
        " Global Test Class
        aunit = ref_scan->aunit_tab[ incl_name = program_name ].
    ENDTRY.

    DATA(line) = get_line_abs( statement-from ).
    result = xsdbool( line IN aunit-line_range ).
  ENDMETHOD.


  METHOD get_tadir_keys.
    DATA(level) = ref_scan->levels[ statement-level ].

    CALL FUNCTION 'TR_TRANSFORM_TRDIR_TO_TADIR'
      EXPORTING
        iv_trdir_name = level-name
      IMPORTING
        es_tadir_keys = result.
  ENDMETHOD.


ENDCLASS.

CLASS y_code_pal_base DEFINITION PUBLIC ABSTRACT
  INHERITING FROM cl_ci_test_scan
  CREATE PUBLIC
  GLOBAL FRIENDS y_code_pal_unit_test_base
                 y_code_pal_coverage.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF message_code,
                 error          TYPE sci_errc VALUE '100',
                 warning        TYPE sci_errc VALUE '101',
                 notification   TYPE sci_errc VALUE '102',
                 not_maintained TYPE sci_errc VALUE '106',
               END OF message_code.

    CONSTANTS: BEGIN OF c_docs_path,
                 main   TYPE string VALUE 'https://github.com/SAP/code-pal-for-abap/blob/master/docs/' ##NO_TEXT,
                 checks TYPE string VALUE 'https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/' ##NO_TEXT,
               END OF c_docs_path.

    DATA: BEGIN OF settings READ-ONLY,
            pseudo_comment                TYPE sci_pcom,
            alternative_pseudo_comment    TYPE sci_pcom,
            disable_on_prodcode_selection TYPE abap_bool,
            disable_on_testcode_selection TYPE abap_bool,
            disable_threshold_selection   TYPE abap_bool,
            object_created_on             TYPE creationdt,
            threshold                     TYPE ycicc_threshold,
            prio                          TYPE ycicc_message_kind,
            apply_on_productive_code      TYPE abap_bool,
            apply_on_test_code            TYPE abap_bool,
            documentation                 TYPE c LENGTH 1000,
            is_threshold_reversed         TYPE abap_bool,
            ignore_pseudo_comments        TYPE abap_bool,
            evaluate_new_child_objects    TYPE abap_bool,
          END OF settings.

    DATA manager TYPE REF TO y_if_code_pal_manager READ-ONLY.

    METHODS constructor.

    METHODS get_attributes REDEFINITION.
    METHODS if_ci_test~display_documentation  REDEFINITION.
    METHODS if_ci_test~query_attributes REDEFINITION.
    METHODS put_attributes  REDEFINITION.
    METHODS run REDEFINITION.

  PROTECTED SECTION.
    CONSTANTS initial_date TYPE datum VALUE '19000101'.

    DATA check_configurations TYPE y_if_code_pal_manager=>check_configurations.
    DATA use_default_attributes TYPE abap_bool VALUE abap_true ##NO_TEXT.

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
                                       RETURNING VALUE(result) TYPE y_if_code_pal_manager=>check_configuration.

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

    METHODS raise_error IMPORTING object_type         TYPE trobjtype DEFAULT c_type_include
                                  statement_level     TYPE stmnt_levl
                                  statement_index     TYPE int4
                                  statement_from      TYPE int4
                                  error_counter       TYPE sci_errcnt OPTIONAL
                                  parameter_01        TYPE csequence OPTIONAL
                                  parameter_02        TYPE csequence OPTIONAL
                                  parameter_03        TYPE csequence OPTIONAL
                                  parameter_04        TYPE csequence OPTIONAL
                                  check_configuration TYPE y_if_code_pal_manager=>check_configuration. "#EC OPTL_PARAM

    METHODS set_check_message IMPORTING message TYPE itex132.

    METHODS condense_tokens IMPORTING statement     TYPE sstmnt
                            RETURNING VALUE(result) TYPE string.

    METHODS is_test_code IMPORTING statement     TYPE sstmnt
                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS add_check_quickfix ABSTRACT IMPORTING check_configuration TYPE y_if_code_pal_manager=>check_configuration
                                                  statement_index     TYPE int4.

    METHODS new_quickfix RETURNING VALUE(result) TYPE REF TO if_ci_quickfix_abap_actions.

    METHODS inform REDEFINITION.

  PRIVATE SECTION.
    METHODS do_attributes_exist  RETURNING VALUE(result) TYPE abap_bool.

    METHODS instantiate_objects.

    METHODS is_config_stricter IMPORTING previous      TYPE y_if_code_pal_manager=>check_configuration
                                         current       TYPE y_if_code_pal_manager=>check_configuration
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS switch_bool IMPORTING boolean       TYPE abap_bool
                        RETURNING VALUE(result) TYPE abap_bool. "#EC BOOL_PARAM #EC METH_RET_BOOL

    METHODS handle_ignore_pseudo_comments IMPORTING  check_configuration TYPE y_if_code_pal_manager=>check_configuration.

    METHODS is_statement_in_aunit_tab IMPORTING statement     TYPE sstmnt
                                      RETURNING VALUE(result) TYPE abap_bool
                                      RAISING   cx_sy_itab_line_not_found.

    METHODS add_pseudo_comment_quickfix IMPORTING check_configuration TYPE y_if_code_pal_manager=>check_configuration
                                                  statement_index     TYPE int4.

ENDCLASS.



CLASS y_code_pal_base IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    manager = NEW y_code_pal_manager( srcid ).

    description = manager->database_access->repository_access->get_class_description( myname ).
    category = 'Y_CODE_PAL_CATEGORY'.
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
    settings-evaluate_new_child_objects = abap_true.

    has_attributes = do_attributes_exist( ).

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-form )
                                        ( scan_struc_stmnt_type-method )
                                        ( scan_struc_stmnt_type-function )
                                        ( scan_struc_stmnt_type-module ) ).

    relevant_structure_types = VALUE #( ( scan_struc_type-event ) ).

    remote_enabled = abap_true.
    remote_rfc_enabled = abap_true.

    INSERT VALUE #( test = myname
                    code = message_code-not_maintained
                    kind = c_note
                    text = TEXT-106 ) INTO TABLE scimessages.
  ENDMETHOD.


  METHOD detect_check_configuration.
    DATA creation_date LIKE settings-object_created_on.

    CHECK check_configurations IS NOT INITIAL.

    DATA(include) = get_include( p_level = statement-level ).
    DATA(is_test_code) = is_test_code( statement ).
    DATA(configurations) = check_configurations.

    IF settings-is_threshold_reversed = abap_true.
      DELETE configurations WHERE threshold < error_count.
    ENDIF.

    IF settings-is_threshold_reversed = abap_false.
      DELETE configurations WHERE threshold > error_count.
    ENDIF.

    IF is_test_code = abap_true.
      DELETE configurations WHERE apply_on_testcode = abap_false.
    ELSE.
      DELETE configurations WHERE apply_on_productive_code = abap_false.
    ENDIF.

    LOOP AT configurations ASSIGNING FIELD-SYMBOL(<configuration>).
      IF is_config_stricter( previous = result
                             current = <configuration> ) = abap_false.
        CONTINUE.
      ENDIF.

      IF result IS INITIAL
      OR result-evaluate_new_child_objects <> <configuration>-evaluate_new_child_objects.
        creation_date = COND #( WHEN <configuration>-evaluate_new_child_objects = abap_true  THEN manager->creation_date->get_creation_date( include )
                                WHEN <configuration>-evaluate_new_child_objects = abap_false THEN manager->creation_date->get_creation_date( get_include( p_level = 1 ) ) ).
      ENDIF.

      IF creation_date < <configuration>-object_creation_date.
        CONTINUE.
      ENDIF.

      no_aunit = xsdbool( <configuration>-apply_on_testcode = abap_false ).
      result = <configuration>.
    ENDLOOP.

    IF result IS INITIAL.
      RETURN.
    ENDIF.

    DATA(exempt) = manager->exemption->is_exempt( object_type = object_type
                                                  object_name = object_name
                                                  include     = include ).

    IF exempt = abap_true
    OR manager->scope->is_it_in_scope( include ) = abap_false.
      CLEAR result.
    ENDIF.
  ENDMETHOD.


  METHOD do_attributes_exist.
    TRY.
        DATA(profiles) = manager->profile->select_profiles( sy-uname ).
        attributes_ok = xsdbool( profiles IS INITIAL ).
      CATCH ycx_code_pal_entry_not_found.
        attributes_ok = abap_true.
    ENDTRY.
    result = attributes_ok.
  ENDMETHOD.


  METHOD execute_check.
    inspect_structures( ).
  ENDMETHOD.


  METHOD inspect_structures.
    LOOP AT FILTER #( ref_scan->structures IN relevant_structure_types WHERE type = table_line ) INTO structure_wa.
      inspect_statements( structure_wa ).
    ENDLOOP.

    LOOP AT FILTER #( ref_scan->structures IN relevant_statement_types WHERE stmnt_type = table_line ) INTO structure_wa.
      inspect_statements( structure_wa ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_statements.
    DATA(index) = structure-stmnt_from.

    LOOP AT ref_scan->statements INTO statement_wa
    FROM structure-stmnt_from TO structure-stmnt_to.
      inspect_tokens( index = index
                      structure = structure
                      statement = statement_wa ).

      index = index + 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_attributes.
    READ TABLE check_configurations INTO DATA(check_configuration) INDEX 1.
    IF sy-subrc <> 0.
      check_configuration-apply_on_productive_code = settings-apply_on_productive_code.
      check_configuration-apply_on_testcode = settings-apply_on_test_code.
      check_configuration-object_creation_date = settings-object_created_on.
      check_configuration-prio = settings-prio.
      check_configuration-threshold = settings-threshold.
      check_configuration-ignore_pseudo_comments = settings-ignore_pseudo_comments.
      check_configuration-evaluate_new_child_objects = settings-evaluate_new_child_objects.
      APPEND check_configuration TO check_configurations.
    ENDIF.
    EXPORT
      object_creation_date = check_configuration-object_creation_date
      message_severity = check_configuration-prio
      threshold = check_configuration-threshold
      apply_on_productive_code = check_configuration-apply_on_productive_code
      apply_on_testcode = check_configuration-apply_on_testcode
      ignore_pseudo_comments = check_configuration-ignore_pseudo_comments
      evaluate_child_objects = check_configuration-evaluate_new_child_objects
    TO DATA BUFFER p_attributes.
  ENDMETHOD.


  METHOD get_code.
    CASE message_prio.
      WHEN c_error.
        result = message_code-error.
      WHEN c_warning.
        result = message_code-warning.
      WHEN c_note.
        result = message_code-notification.
      WHEN OTHERS.
        result = message_code-not_maintained.
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
    DATA sci_attributes TYPE sci_atttab.
    DATA message(72) TYPE c.

    READ TABLE check_configurations INTO DATA(check_configuration) INDEX 1.
    IF sy-subrc <> 0 AND use_default_attributes = abap_true.
      check_configuration-object_creation_date = settings-object_created_on.
      check_configuration-prio = settings-prio.
      check_configuration-apply_on_productive_code = settings-apply_on_productive_code.
      check_configuration-apply_on_testcode = settings-apply_on_test_code.
      check_configuration-threshold = settings-threshold.
      check_configuration-ignore_pseudo_comments = settings-ignore_pseudo_comments.
      check_configuration-evaluate_new_child_objects = settings-evaluate_new_child_objects.
    ENDIF.

    INSERT VALUE #(
      kind = ''
      ref  = REF #( check_configuration-object_creation_date )
      text =  'Consider Objects created after'(200)
    ) INTO TABLE sci_attributes.

    INSERT VALUE #(
      kind = ''
      ref  = REF #( check_configuration-prio )
      text =  'Message Severity'(201)
    ) INTO TABLE sci_attributes.

    IF settings-disable_threshold_selection = abap_false.
      INSERT VALUE #(
        kind = ''
        ref  = REF #( check_configuration-threshold )
        text =  'Threshold'(203)
      ) INTO TABLE sci_attributes.
    ENDIF.

    IF settings-disable_on_prodcode_selection = abap_false.
      INSERT VALUE #(
        kind = ''
        ref  = REF #( check_configuration-apply_on_productive_code )
        text =  'Apply on Productive Code'(204)
      ) INTO TABLE sci_attributes.
    ENDIF.

    IF settings-disable_on_testcode_selection = abap_false.
      INSERT VALUE #(
        kind = ''
        ref  = REF #( check_configuration-apply_on_testcode )
        text =  'Apply on Testcode'(202)
      ) INTO TABLE sci_attributes.
    ENDIF.

    check_configuration-ignore_pseudo_comments = switch_bool( check_configuration-ignore_pseudo_comments ).

    IF settings-pseudo_comment IS NOT INITIAL.
      INSERT VALUE #(
        kind = ''
        ref  = REF #( check_configuration-ignore_pseudo_comments )
        text = |Allow { settings-pseudo_comment }|
      ) INTO TABLE sci_attributes.
    ENDIF.

    INSERT VALUE #(
      kind = ''
      ref  = REF #( check_configuration-evaluate_new_child_objects )
      text = |Evaluate Child Objects|
    ) INTO TABLE sci_attributes.

    attributes_ok = abap_false.
    WHILE attributes_ok = abap_false.
      IF cl_ci_query_attributes=>generic(
                         p_name       = name
                         p_title      = |{ description }|
                         p_attributes = sci_attributes
                         p_message    = message
                         p_display    = p_display ) = abap_true.
        attributes_ok = abap_true.
        check_configuration-ignore_pseudo_comments = switch_bool( check_configuration-ignore_pseudo_comments ).
        RETURN.
      ENDIF.

      IF check_configuration-apply_on_productive_code = abap_false AND
         check_configuration-apply_on_testcode        = abap_false.
        message = 'Choose the Type of Code to be checked'(300).
      ELSEIF check_configuration-prio IS INITIAL.
        message = 'Choose a Message Severity'(301).
      ELSE.
        IF check_configuration-object_creation_date IS INITIAL.
          check_configuration-object_creation_date = initial_date.
        ENDIF.

        attributes_ok = abap_true.
      ENDIF.
    ENDWHILE.

    check_configuration-ignore_pseudo_comments = switch_bool( check_configuration-ignore_pseudo_comments ).

    CLEAR check_configurations.
    APPEND check_configuration TO check_configurations.
    use_default_attributes = abap_false.
  ENDMETHOD.


  METHOD instantiate_objects.
    IF ref_scan IS INITIAL.
      no_aunit = abap_true.
      get( ).
    ENDIF.

    quickfix_factory = cl_ci_quickfix_creation=>create_quickfix_alternatives( ).

    manager->set_scope( ref_scan->levels[ 1 ]-name ).
  ENDMETHOD.


  METHOD put_attributes.
    DATA check_configuration TYPE y_if_code_pal_manager=>check_configuration.

    TRY.
        IMPORT
          object_creation_date = check_configuration-object_creation_date
          message_severity = check_configuration-prio
          threshold = check_configuration-threshold
          apply_on_productive_code = check_configuration-apply_on_productive_code
          apply_on_testcode = check_configuration-apply_on_testcode
          ignore_pseudo_comments = check_configuration-ignore_pseudo_comments
          evaluate_child_objects = check_configuration-evaluate_new_child_objects
        FROM DATA BUFFER p_attributes.
        APPEND check_configuration TO check_configurations.
      CATCH cx_root.                                  "#EC NEED_CX_ROOT
        attributes_ok = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD raise_error.
    " Probably, the Profile is not relevant (threshold, dates, etc)
    CHECK check_configuration IS NOT INITIAL.

    handle_ignore_pseudo_comments( check_configuration ).

    add_pseudo_comment_quickfix( check_configuration = check_configuration
                                 statement_index     = statement_index ).

    add_check_quickfix( check_configuration = check_configuration
                        statement_index     = statement_index ).

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
            p_detail = quickfix_factory->export_to_xstring( ) ).
  ENDMETHOD.


  METHOD run.
    instantiate_objects( ).

    IF ref_scan IS INITIAL
    OR ref_scan->subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    " SCI variant
    IF has_attributes = abap_true
    AND attributes_ok = abap_false.
      raise_error( statement_level = 1
                   statement_index = 1
                   statement_from  = 1
                   check_configuration = VALUE #( prio = `E` ) ).
      RETURN.
    ENDIF.

    " Profile
    IF has_attributes = abap_false.
      TRY.
          check_configurations = manager->get_profile_configuration( myname ).
        CATCH ycx_code_pal_no_customizing.
          RETURN.
      ENDTRY.
    ENDIF.

    " No relevant configurations
    IF check_configurations IS INITIAL.
      RETURN.
    ENDIF.

    execute_check( ).
  ENDMETHOD.


  METHOD set_check_message.
    DATA(pseudo_comment) = COND #( WHEN settings-pseudo_comment IS NOT INITIAL
                                   THEN settings-pseudo_comment+5 ).

    DATA(alternative_pseudo_comment) = COND #( WHEN settings-alternative_pseudo_comment IS NOT INITIAL
                                               THEN settings-alternative_pseudo_comment+5 ).

    DATA(error) = VALUE scimessage( kind = c_error
                                    code = get_code( c_error )
                                    test = myname
                                    text = message
                                    pcom = pseudo_comment
                                    pcom_alt = alternative_pseudo_comment ).

    DATA(warning) = VALUE scimessage( kind = c_warning
                                      code = get_code( c_warning )
                                      test = myname
                                      text = message
                                      pcom = pseudo_comment
                                      pcom_alt = alternative_pseudo_comment ).

    DATA(notification) = VALUE scimessage( kind = c_note
                                           code = get_code( c_note )
                                           test = myname
                                           text = message
                                           pcom = pseudo_comment
                                           pcom_alt = alternative_pseudo_comment ).

    INSERT error INTO TABLE scimessages.
    INSERT warning INTO TABLE scimessages.
    INSERT notification INTO TABLE scimessages.
  ENDMETHOD.


  METHOD is_config_stricter.
    DATA(threshold_stricter) = xsdbool( ( previous-threshold >= current-threshold AND settings-is_threshold_reversed = abap_false )
                                     OR ( previous-threshold < current-threshold AND settings-is_threshold_reversed = abap_true ) ).

    result = xsdbool( ( previous IS INITIAL )
                   OR ( previous-prio = current-prio AND threshold_stricter = abap_true )
                   OR ( previous-prio <> c_error AND current-prio = c_error )
                   OR ( previous-prio = c_note AND current-prio = c_warning )
                   OR ( previous-ignore_pseudo_comments = abap_false AND current-ignore_pseudo_comments = abap_true )
                   OR ( previous-evaluate_new_child_objects = abap_false AND current-evaluate_new_child_objects = abap_true ) ).
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


  METHOD add_pseudo_comment_quickfix.
    CHECK settings-pseudo_comment IS NOT INITIAL.
    CHECK check_configuration-ignore_pseudo_comments = abap_false.

    TRY.
        DATA(quickfix) = CAST if_ci_quickfix_abap_actions( quickfix_factory->create_quickfix( ) ).

        quickfix->add_pseudo_comment( p_pseudo_comment = settings-pseudo_comment+5
                                      p_context        = cl_ci_quickfix_abap_context=>create_from_scan_stmt( p_ci_scan = ref_scan
                                                                                                             p_stmt_idx = statement_index ) ).
      CATCH cx_ci_quickfix_failed.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD new_quickfix.
    DATA(quickfix) = quickfix_factory->create_quickfix( ).

    quickfix->add_docu_from_msgclass( p_msg_class = 'Y_CODE_PAL_MESSAGES'
                                      p_msg_number = '001'
                                      p_msg_parameter1 = description ).

    quickfix->enable_automatic_execution( ).

    result = CAST #( quickfix ).
  ENDMETHOD.


  METHOD inform.
    IF manager->statistics IS BOUND.
      manager->statistics->collect( ref_scan    = ref_scan
                                    scimessages = scimessages
                                    test        = p_test
                                    code        = p_code
                                    kind        = p_kind
                                    suppress    = p_suppress
                                    position    = p_position ).
    ELSE.
      super->inform( p_sub_obj_type    = p_sub_obj_type
                     p_sub_obj_name    = p_sub_obj_name
                     p_position        = p_position
                     p_line            = p_line
                     p_column          = p_column
                     p_errcnt          = p_errcnt
                     p_kind            = p_kind
                     p_test            = p_test
                     p_code            = p_code
                     p_suppress        = p_suppress
                     p_param_1         = p_param_1
                     p_param_2         = p_param_2
                     p_param_3         = p_param_3
                     p_param_4         = p_param_4
                     p_inclspec        = p_inclspec
                     p_detail          = p_detail
                     p_checksum_1      = p_checksum_1
                     p_comments        = p_comments
                     p_finding_origins = p_finding_origins ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.

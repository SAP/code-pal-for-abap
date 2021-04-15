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
    DATA is_testcode TYPE abap_bool.
    DATA ref_scan_manager TYPE REF TO y_if_scan_manager.
    DATA statistics TYPE REF TO y_if_scan_statistics.
    DATA test_code_detector TYPE REF TO y_if_testcode_detector.
    DATA use_default_attributes TYPE abap_bool VALUE abap_true ##NO_TEXT.
    DATA attributes_maintained TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Relevant Statement Types for Inspection</p>
    "! There are default values set in the Y_CHECK_BASE, and you can reuse the constants available in the 'scan_struc_stmnt_type' structure to enhance or change it.
    DATA relevant_statement_types TYPE TABLE OF sstruc-stmnt_type.

    "! <p class="shorttext synchronized" lang="en">Relevant Structure Types for Inspection</p>
    "! There are default values set in the Y_CHECK_BASE, and you can reuse the constants available in the 'scan_struc_type' structure to enhance or change it.
    DATA relevant_structure_types TYPE TABLE OF sstruc-type.

    METHODS execute_check.

    METHODS check_start_conditions RAISING ycx_object_not_processed.

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
                                  error_priority         TYPE sychar01
                                  parameter_01           TYPE csequence OPTIONAL
                                  parameter_02           TYPE csequence OPTIONAL
                                  parameter_03           TYPE csequence OPTIONAL
                                  parameter_04           TYPE csequence OPTIONAL
                                  is_include_specific    TYPE sci_inclspec DEFAULT ' '
                                  additional_information TYPE xstring OPTIONAL
                                  checksum               TYPE int4 OPTIONAL. "#EC OPTL_PARAM

    METHODS get_column_abs  REDEFINITION.
    METHODS get_column_rel REDEFINITION.
    METHODS get_include  REDEFINITION.
    METHODS get_line_abs REDEFINITION.
    METHODS get_line_column_abs REDEFINITION.
    METHODS get_line_column_rel  REDEFINITION.
    METHODS get_line_rel REDEFINITION.
    METHODS get_token_abs REDEFINITION.
    METHODS get_token_rel REDEFINITION.
    METHODS keyword REDEFINITION.
    METHODS set_check_message IMPORTING message TYPE itex132.
    METHODS get_class_description  RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    METHODS do_attributes_exist  RETURNING VALUE(result) TYPE abap_bool.

    METHODS instantiate_objects.

    METHODS is_skipped IMPORTING config        TYPE y_if_clean_code_manager=>check_configuration
                                 error_count   TYPE int4
                       RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_treshold_config_valid IMPORTING previous_threshold TYPE int4
                                               config_threshold   TYPE int4
                                     RETURNING VALUE(result)      TYPE abap_bool.

    METHODS is_config_setup_valid IMPORTING previous_config TYPE y_if_clean_code_manager=>check_configuration
                                            config          TYPE y_if_clean_code_manager=>check_configuration
                                  RETURNING VALUE(result)   TYPE abap_bool.

    METHODS should_skip_test_code IMPORTING structure     TYPE sstruc
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS should_skip_type IMPORTING structure     TYPE sstruc
                             RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_statement_type_relevant IMPORTING structure     TYPE sstruc
                                       RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_structure_type_relevant IMPORTING structure     TYPE sstruc
                                       RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_app_comp_in_scope IMPORTING level         TYPE stmnt_levl
                                 RETURNING VALUE(result) TYPE abap_bool.

    METHODS switch_bool IMPORTING boolean       TYPE abap_bool
                        RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_CHECK_BASE IMPLEMENTATION.


  METHOD check_start_conditions.
    IF ref_scan_manager->is_scan_ok( ) = abap_false.
      RAISE EXCEPTION TYPE ycx_object_not_processed.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

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

    has_attributes = do_attributes_exist( ).

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


  METHOD detect_check_configuration.
    DATA tadir_keys TYPE tadir.

    DATA(level) = ref_scan_manager->levels[ statement-level ].

    CALL FUNCTION 'TR_TRANSFORM_TRDIR_TO_TADIR'
      EXPORTING
        iv_trdir_name = level-name
      IMPORTING
        es_tadir_keys = tadir_keys.

    DATA(creation_date) = clean_code_manager->calculate_obj_creation_date( object_type = tadir_keys-object
                                                                           object_name = tadir_keys-obj_name  ).

    LOOP AT check_configurations ASSIGNING FIELD-SYMBOL(<configuration>)
    WHERE object_creation_date <= creation_date.

      IF is_skipped( config      = <configuration>
                     error_count = error_count ) = abap_true.
        CONTINUE.
      ENDIF.

      IF result IS INITIAL
         OR is_config_setup_valid( previous_config = result
                                   config          = <configuration> ) = abap_true.
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


  METHOD do_attributes_exist.
    TRY.
        DATA(profiles) = y_profile_manager=>create( )->select_profiles( sy-uname ).
        attributes_ok = xsdbool( profiles IS INITIAL ).
      CATCH ycx_entry_not_found.
        attributes_ok = abap_true.
    ENDTRY.
    result = attributes_ok.
  ENDMETHOD.


  METHOD execute_check.
    inspect_structures( ).
  ENDMETHOD.


  METHOD inspect_structures.
    LOOP AT ref_scan_manager->structures ASSIGNING FIELD-SYMBOL(<structure>).
      IF should_skip_type( <structure> ) = abap_true
      OR should_skip_test_code( <structure> ) = abap_true.
        CONTINUE.
      ENDIF.

      inspect_statements( <structure> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_statements.
    DATA(index) = structure-stmnt_from.

    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from
    TO structure-stmnt_to.
      inspect_tokens( index = index
                      structure = structure
                      statement = <statement> ).

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
      APPEND check_configuration TO check_configurations.
    ENDIF.
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


  METHOD get_column_abs.
    DATA(tokens) = ref_scan_manager->tokens.
    IF lines( tokens ) = 0.
      RETURN.
    ENDIF.

    DO.
      READ TABLE tokens INDEX p_n ASSIGNING FIELD-SYMBOL(<token>).
      IF sy-subrc = 0 AND <token>-row <> 0.
        p_result = <token>-col.
        RETURN.
      ENDIF.
      p_n = p_n - 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_column_rel.
    DATA(index) = statement_wa-from + p_n - 1.
    CHECK index <= statement_wa-to.

    DATA(tokens) = ref_scan_manager->tokens.
    IF lines( tokens ) = 0.
      RETURN.
    ENDIF.

    DO.
      READ TABLE tokens INDEX index ASSIGNING FIELD-SYMBOL(<token>).
      IF sy-subrc = 0 AND <token>-row <> 0.
        p_result = <token>-col.
        RETURN.
      ENDIF.
      index = index - 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_include.
    DATA(l_level) = COND #( WHEN p_level IS SUPPLIED THEN p_level
                            ELSE statement_wa-level ).

    DO.
      READ TABLE ref_scan_manager->levels INDEX l_level INTO DATA(l_levels_wa).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      IF l_levels_wa-type = 'P'.
        p_result = l_levels_wa-name.
        RETURN.
      ENDIF.
      l_level = l_levels_wa-level.
    ENDDO.
  ENDMETHOD.


  METHOD get_line_abs.
    DATA(tokens) = ref_scan_manager->tokens.
    IF lines( tokens ) = 0.
      RETURN.
    ENDIF.

    DO.
      READ TABLE tokens INDEX p_n ASSIGNING FIELD-SYMBOL(<token>).
      IF sy-subrc = 0 AND <token>-row <> 0.
        p_result = <token>-row.
        RETURN.
      ENDIF.
      p_n = p_n - 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_line_column_abs.
    DATA(tokens) = ref_scan_manager->tokens.
    IF lines( tokens ) = 0.
      RETURN.
    ENDIF.

    DO.
      READ TABLE tokens INDEX p_n ASSIGNING FIELD-SYMBOL(<token>).
      IF sy-subrc = 0 AND <token>-row <> 0.
        p_column = <token>-col.
        p_line   = <token>-row.
        RETURN.
      ENDIF.
      p_n = p_n - 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_line_column_rel.
    DATA(tokens) = ref_scan_manager->tokens.
    IF lines( tokens ) = 0.
      RETURN.
    ENDIF.

    p_n = statement_wa-from + p_n - 1.

    DO.
      READ TABLE tokens INDEX p_n ASSIGNING FIELD-SYMBOL(<token>).
      IF sy-subrc = 0 AND <token>-row <> 0.
        p_column = <token>-col.
        p_line   = <token>-row.
        RETURN.
      ENDIF.
      p_n = p_n - 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_line_rel.
    DATA(index) = statement_wa-from + p_n - 1.
    CHECK index <= statement_wa-to.

    DATA(tokens) = ref_scan_manager->tokens.
    IF lines( tokens ) = 0.
      RETURN.
    ENDIF.

    DO.
      READ TABLE tokens INDEX index ASSIGNING FIELD-SYMBOL(<token>).
      IF sy-subrc = 0 AND <token>-row <> 0.
        p_result = <token>-row.
        RETURN.
      ENDIF.
      index = index - 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_token_abs.
    READ TABLE ref_scan_manager->tokens INDEX p_n INTO token_wa.
    IF sy-subrc = 0.
      p_result = token_wa-str.
    ENDIF.
  ENDMETHOD.


  METHOD get_token_rel.
    DATA(l_index) = statement_wa-from + p_n - 1.
    IF l_index > statement_wa-to.
      RETURN.
    ENDIF.
    READ TABLE ref_scan_manager->tokens INDEX l_index INTO token_wa.
    p_result = token_wa-str.
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
    DATA title(75) TYPE c.
    DATA message(72) TYPE c.

    READ TABLE check_configurations INTO DATA(check_configuration) INDEX 1.
    IF sy-subrc <> 0 AND use_default_attributes = abap_true.
      check_configuration-object_creation_date = settings-object_created_on.
      check_configuration-prio = settings-prio.
      check_configuration-apply_on_productive_code = settings-apply_on_productive_code.
      check_configuration-apply_on_testcode = settings-apply_on_test_code.
      check_configuration-threshold = settings-threshold.
      check_configuration-ignore_pseudo_comments = settings-ignore_pseudo_comments.
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

    title = description.

    attributes_ok = abap_false.
    WHILE attributes_ok = abap_false.
      IF cl_ci_query_attributes=>generic(
                         p_name       = name
                         p_title      = title
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
    IF ref_scan_manager IS NOT BOUND.
      ref_scan_manager = NEW y_ref_scan_manager( ).
      IF ref_scan IS INITIAL.
        get( ).
      ENDIF.
    ENDIF.
    ref_scan_manager->set_ref_scan( ref_scan ).

    IF clean_code_manager IS NOT BOUND.
      clean_code_manager = NEW y_clean_code_manager( ).
    ENDIF.

    IF clean_code_exemption_handler IS NOT BOUND.
      clean_code_exemption_handler = NEW y_exemption_handler( ).
    ENDIF.

    IF test_code_detector IS NOT BOUND.
      test_code_detector = NEW y_test_code_detector( ).
    ENDIF.
    test_code_detector->clear( ).
    test_code_detector->set_ref_scan_manager( ref_scan_manager ).

    IF statistics IS NOT BOUND.
      statistics = NEW y_scan_statistics( ).
    ENDIF.

    IF lines( check_configurations ) = 1
    AND check_configurations[ 1 ]-object_creation_date IS INITIAL.
      CLEAR check_configurations.
    ENDIF.
  ENDMETHOD.


  METHOD keyword.
    IF statement_wa-type = 'C'.
      p_result = 'COMPUTE'.
      RETURN.
    ENDIF.
    READ TABLE ref_scan_manager->tokens INDEX statement_wa-from INTO token_wa.
    p_result = token_wa-str.
  ENDMETHOD.


  METHOD put_attributes.
    DATA check_configuration TYPE y_if_clean_code_manager=>check_configuration.

    attributes_maintained = abap_true.
    TRY.
        IMPORT
          object_creation_date = check_configuration-object_creation_date
          message_severity = check_configuration-prio
          threshold = check_configuration-threshold
          apply_on_productive_code = check_configuration-apply_on_productive_code
          apply_on_testcode = check_configuration-apply_on_testcode
          ignore_pseudo_comments = check_configuration-ignore_pseudo_comments
        FROM DATA BUFFER p_attributes.
        APPEND check_configuration TO check_configurations.
      CATCH cx_root.                                  "#EC NEED_CX_ROOT
        attributes_maintained = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD raise_error.
    DATA(pseudo_comment) = COND #( WHEN settings-ignore_pseudo_comments = abap_false THEN settings-pseudo_comment ).
    DATA(pcom_detector) = NEW y_pseudo_comment_detector( )->is_pseudo_comment( ref_scan_manager = ref_scan_manager
                                                                               scimessages = scimessages
                                                                               test = myname
                                                                               code = get_code( error_priority )
                                                                               suppress = pseudo_comment
                                                                               position = statement_index ).
    statistics->collect( kind = error_priority
                         pc = pcom_detector ).

    IF cl_abap_typedescr=>describe_by_object_ref( ref_scan_manager )->get_relative_name( ) = 'Y_REF_SCAN_MANAGER'.
      inform( p_sub_obj_type = object_type
              p_sub_obj_name = get_include( p_level = statement_level )
              p_position = statement_index
              p_line = get_line_abs( statement_from )
              p_column = get_column_abs( statement_from )
              p_errcnt = error_counter
              p_kind = error_priority
              p_test = myname
              p_code = get_code( error_priority )
              p_suppress = pseudo_comment
              p_param_1 = parameter_01
              p_param_2 = parameter_02
              p_param_3 = parameter_03
              p_param_4 = parameter_04
              p_inclspec = is_include_specific
              p_detail = additional_information
              p_checksum_1 = checksum ).
    ENDIF.
  ENDMETHOD.


  METHOD run.
    DATA profile_configurations TYPE y_if_clean_code_manager=>check_configurations.

    instantiate_objects( ).

    IF attributes_maintained = abap_false AND has_attributes = abap_true.
      raise_error( statement_level = 1
                   statement_index = 1
                   statement_from  = 1
                   error_priority  = '' ).
      FREE ref_scan_manager.
      RETURN.
    ENDIF.

    TRY.
        check_start_conditions( ).
        profile_configurations = clean_code_manager->read_check_customizing( myname ).
      CATCH ycx_no_check_customizing.
        IF profile_configurations IS INITIAL AND attributes_ok = abap_false.
          FREE ref_scan_manager.
          RETURN.
        ELSEIF attributes_ok = abap_true.
          profile_configurations = check_configurations.
        ENDIF.
      CATCH ycx_object_not_processed.
        FREE ref_scan_manager.
        RETURN.

    ENDTRY.

    IF lines( profile_configurations ) > 0.
      check_configurations = profile_configurations.
    ENDIF.

    execute_check( ).

    FREE ref_scan_manager.
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


  METHOD is_config_setup_valid.
    result = xsdbool( ( previous_config-prio = config-prio
                       AND is_treshold_config_valid( config_threshold = config-threshold
                                                     previous_threshold = previous_config-threshold ) = abap_true )
                     OR ( previous_config-prio <> c_error AND config-prio = c_error )
                     OR ( previous_config-prio = c_note AND config-prio = c_warning )
                     OR ( previous_config-ignore_pseudo_comments = abap_false
                         AND config-ignore_pseudo_comments = abap_true ) ).
  ENDMETHOD.


  METHOD is_skipped.
    result = xsdbool( ( config-threshold < error_count AND settings-is_threshold_reversed = abap_true )
                     OR ( config-threshold > error_count AND settings-is_threshold_reversed = abap_false )
                     OR ( is_testcode = abap_true AND config-apply_on_testcode = abap_false )
                     OR ( is_testcode = abap_false AND config-apply_on_productive_code = abap_false ) ).
  ENDMETHOD.


  METHOD is_treshold_config_valid.
    result = xsdbool( ( previous_threshold >= config_threshold AND settings-is_threshold_reversed = abap_false )
                     OR ( previous_threshold < config_threshold AND settings-is_threshold_reversed = abap_true ) ).
  ENDMETHOD.


  METHOD should_skip_test_code.
    " From Code Inspector (required)
    is_testcode = test_code_detector->is_testcode( structure ).

    DATA(has_customizing_for_prod_only) = xsdbool( NOT line_exists( check_configurations[ apply_on_testcode = abap_true ] ) ).

    result = xsdbool(     has_customizing_for_prod_only = abap_true
                      AND is_testcode = abap_true ).
  ENDMETHOD.


  METHOD should_skip_type.
    result = xsdbool(     is_statement_type_relevant( structure ) = abap_false
                      AND is_structure_type_relevant( structure ) = abap_false ).
  ENDMETHOD.


  METHOD is_statement_type_relevant.
    result = xsdbool( line_exists( relevant_statement_types[ table_line = structure-stmnt_type ] ) ).
  ENDMETHOD.


  METHOD is_structure_type_relevant.
    result = xsdbool( line_exists( relevant_structure_types[ table_line = structure-type ] ) ).
  ENDMETHOD.


  METHOD is_app_comp_in_scope.
    TRY.
        DATA(main_app_comp) = y_code_pal_app_comp=>get( ref_scan_manager->levels[ level = 0 ] ).
        DATA(curr_app_comp) = y_code_pal_app_comp=>get( ref_scan_manager->levels[ level ] ).
        result = xsdbool( main_app_comp = curr_app_comp ).
      CATCH cx_sy_itab_line_not_found
            ycx_entry_not_found.
        result = abap_true.
    ENDTRY.
  ENDMETHOD.


  METHOD switch_bool.
    result = xsdbool( boolean = abap_false ).
  ENDMETHOD.
ENDCLASS.

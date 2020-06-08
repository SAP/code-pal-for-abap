CLASS y_check_function DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE seoclsname VALUE 'Y_CHECK_FUNCTION' ##NO_TEXT.
    CONSTANTS second_token TYPE i VALUE 2 ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.

    DATA db_reader TYPE REF TO lif_db_reader .
ENDCLASS.



CLASS Y_CHECK_FUNCTION IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'FUNCTION Module Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '360'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CI_FUNCTION' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }function-routine.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Function-Module should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD execute_check.
    IF db_reader IS NOT BOUND.
      db_reader = NEW lcl_db_reader( ).
    ENDIF.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-function.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_config) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      READ TABLE ref_scan_manager->get_statements( ) INDEX <structure>-stmnt_to INTO statement_for_message.

      IF NOT db_reader->is_fm_rfc_enabled( get_token_rel( second_token ) ).
        DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                                include = get_include( p_level = statement_for_message-level ) ).
        IF check_configuration IS INITIAL.
          CONTINUE.
        ENDIF.

        raise_error( p_sub_obj_type = c_type_include
                     p_level        = statement_for_message-level
                     p_position     = <structure>-stmnt_to
                     p_from         = statement_for_message-from
                     p_kind         = check_configuration-prio
                     p_test         = me->myname
                     p_code         = get_code( check_configuration-prio ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS y_check_form DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_my_name TYPE seoclsname VALUE 'Y_CHECK_FORM' ##NO_TEXT.

    METHODS constructor .

  PROTECTED SECTION.

    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CHECK_FORM IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'FORM Routine Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '330'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CI_FORM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }form-routine.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: "FORM" Routine should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-form.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_config) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      READ TABLE ref_scan_manager->get_statements( ) INDEX <structure>-stmnt_to INTO DATA(statement_for_message).

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
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

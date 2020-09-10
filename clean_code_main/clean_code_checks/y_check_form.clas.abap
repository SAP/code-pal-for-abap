CLASS y_check_form DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

  PROTECTED SECTION.

    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CHECK_FORM IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_FORM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }form-routine.md|.

    set_check_message( '[Clean Code]: "FORM" Routine should not be used!' ).
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

      DATA(check_configuration) = detect_check_configuration( statement_for_message ).

      IF check_configuration IS INITIAL.
        CONTINUE.
      ENDIF.

      raise_error( statement_level     = statement_for_message-level
                   statement_index     = <structure>-stmnt_to
                   statement_from      = statement_for_message-from
                   error_priority      = check_configuration-prio ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

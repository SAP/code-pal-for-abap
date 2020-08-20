CLASS y_check_method_output_param DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS execute_check REDEFINITION.

  PRIVATE SECTION.
    DATA has_found_methods TYPE abap_bool.
    DATA has_exporting_parameter TYPE abap_bool.
    DATA has_changing_parameter TYPE abap_bool.
    DATA has_returning_parameter TYPE abap_bool.
    DATA has_pseudo_comment TYPE abap_bool.

    METHODS check_token_content
      IMPORTING token TYPE stokesx.

    METHODS calculate_param_combination RETURNING VALUE(result) TYPE i.
ENDCLASS.



CLASS Y_CHECK_METHOD_OUTPUT_PARAM IMPLEMENTATION.


  METHOD calculate_param_combination.
    IF has_exporting_parameter = abap_true.
      ADD 1 TO result.
    ENDIF.
    IF has_changing_parameter = abap_true.
      ADD 1 TO result.
    ENDIF.
    IF has_returning_parameter = abap_true.
      ADD 1 TO result.
    ENDIF.
  ENDMETHOD.


  METHOD check_token_content.
    IF token-str EQ 'EXPORTING'.
      has_exporting_parameter = abap_true.
    ENDIF.
    IF token-str EQ 'CHANGING'.
      has_changing_parameter = abap_true.
    ENDIF.
    IF token-str EQ'RETURNING'.
      has_returning_parameter = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    description = 'Combination of Output Parameters'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '090'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC PARAMETER_OUT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 1.
    settings-documentation = |{ c_docs_path-checks }method-output-parameter.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Combination of parameters(RETURNING/EXPORTING) should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition OR
            stmnt_type EQ scan_struc_stmnt_type-interface.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      DATA(index) = <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.
        inspect_tokens( index = index
                        statement = <statement> ).
        index = index + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHODS' OR
          get_token_abs( statement-from ) = 'CLASS-METHODS'.

    has_exporting_parameter = abap_false.
    has_changing_parameter = abap_false.
    has_returning_parameter = abap_false.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      CASE <token>-str.
        WHEN 'EXPORTING'.
          has_exporting_parameter = abap_true.
        WHEN 'RETURNING'.
          has_returning_parameter = abap_true.
        WHEN 'CHANGING'.
          has_changing_parameter = abap_true.
      ENDCASE.

    ENDLOOP.

    DATA(check_configuration) = detect_check_configuration( error_count = 1
                                                            include = get_include( p_level = statement-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF calculate_param_combination( ) > check_configuration-threshold.
      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

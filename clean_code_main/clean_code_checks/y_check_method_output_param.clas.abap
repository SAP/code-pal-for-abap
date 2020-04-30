CLASS y_check_method_output_param DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_METHOD_OUTPUT_PARAM' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS execute_check REDEFINITION.

  PRIVATE SECTION.
    DATA has_exporting_parameter TYPE abap_bool VALUE abap_false.
    DATA has_changing_parameter TYPE abap_bool VALUE abap_false.
    DATA has_returning_parameter TYPE abap_bool VALUE abap_false.
    DATA has_pseudo_comment TYPE abap_bool VALUE abap_false.

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
    DATA(method_index) = statement-from.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      IF <token>-str NE 'METHODS' AND <token>-str NE 'CLASS-METHODS'.
        method_index = method_index + 1.
        CONTINUE.
      ENDIF.

      has_exporting_parameter = abap_false.
      has_changing_parameter = abap_false.
      has_returning_parameter = abap_false.

      statement_for_message = statement.

      DATA(check_configuration) = detect_check_configuration( threshold = 1
                                                              include = get_include( p_level = statement_for_message-level ) ).
      IF check_configuration IS INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<method_token>)
        FROM method_index + 1 TO statement-to.

        CASE <method_token>-str.
          WHEN 'METHODS' OR 'CLASS-METHODS'.
            EXIT.
        ENDCASE.

        check_token_content( <method_token> ).

      ENDLOOP.

      IF calculate_param_combination( ) > check_configuration-threshold.
        raise_error( p_sub_obj_type = c_type_include
                     p_level        = statement_for_message-level
                     p_position     = method_index
                     p_from         = statement_for_message-from
                     p_kind         = check_configuration-prio
                     p_test         = me->myname
                     p_code         = get_code( check_configuration-prio )
                     p_suppress     = settings-pseudo_comment ).
      ENDIF.

      method_index = method_index + 1.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

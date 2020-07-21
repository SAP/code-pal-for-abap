CLASS y_check_num_output_parameter DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_NUM_OUTPUT_PARAMETER' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS execute_check REDEFINITION.

  PRIVATE SECTION.
    DATA number_of_output_parameters TYPE i VALUE 0.
    DATA is_region_blocked TYPE abap_bool VALUE abap_false.

    METHODS is_token_output_type
      IMPORTING token     TYPE stokesx
                threshold TYPE i.
    METHODS is_token_parameter IMPORTING token TYPE stokesx.
    METHODS is_token_importing IMPORTING token TYPE stokesx.
ENDCLASS.



CLASS Y_CHECK_NUM_OUTPUT_PARAMETER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    description = 'Number of Output Parameters'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '690'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC NUM_OUTPUT_PARA' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 1.
    settings-documentation = |{ c_docs_path-checks }number-output-parameter.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Too many output parameters!'(102)
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

      statement_for_message = statement.
      number_of_output_parameters = 0.

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

        is_token_importing( <method_token> ).
        is_token_output_type( token = <method_token>
                              threshold = check_configuration-threshold ).
        is_token_parameter( <method_token> ).

      ENDLOOP.

      IF number_of_output_parameters > check_configuration-threshold.
        raise_error( p_sub_obj_type = c_type_include
                     p_level        = statement_for_message-level
                     p_position     = method_index
                     p_from         = statement_for_message-from
                     p_kind         = check_configuration-prio
                     p_test         = me->myname
                     p_code         = get_code( check_configuration-prio ) ).
      ENDIF.

      method_index = method_index + 1.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_token_importing.
    IF token-str  = 'IMPORTING'.
      is_region_blocked = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_token_output_type.
    CASE token-str.
      WHEN 'EXPORTING' OR 'CHANGING' OR 'RETURNING'.
        IF number_of_output_parameters <= threshold.
          number_of_output_parameters = 0.
        ENDIF.
        is_region_blocked = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD is_token_parameter.
    CHECK is_region_blocked = abap_false.
    CASE token-str.
      WHEN 'TYPE' OR 'LIKE'.
        ADD 1 TO number_of_output_parameters.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS y_check_method_return_bool DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC.

  PUBLIC SECTION.

    DATA method_name TYPE string.

    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS execute_check REDEFINITION.

  PRIVATE SECTION.

    DATA good_method_names_beginning TYPE TABLE OF string.
    DATA good_method_names_containing TYPE TABLE OF string.

    METHODS contains_name_condition
      IMPORTING
        !stmnt_index  TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_CHECK_METHOD_RETURN_BOOL IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Method Name misleading for Boolean Return Type'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '480'.
    version = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC METH_RET_BOOL' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-prio = 'W'.
    settings-documentation = |{ c_docs_path-checks }method-return-bool.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Method &1 has a misleading name for boolean return type!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).

    APPEND 'IS_'     TO good_method_names_beginning.
    APPEND 'HAS_'    TO good_method_names_beginning.
    APPEND 'ARE_'    TO good_method_names_beginning.
    APPEND 'TRY_'    TO good_method_names_beginning.
    APPEND 'CAN_'    TO good_method_names_beginning.
    APPEND 'HAVE_'   TO good_method_names_beginning.
    APPEND 'MUST_'   TO good_method_names_beginning.
    APPEND 'STARTS_' TO good_method_names_beginning.
    APPEND 'ENDS_'   TO good_method_names_beginning.
    APPEND 'SHOULD_' TO good_method_names_beginning.

    APPEND 'EXIST'   TO good_method_names_containing.
    APPEND 'EQUAL'   TO good_method_names_containing.
    APPEND 'CONTAIN' TO good_method_names_containing.
  ENDMETHOD.


  METHOD contains_name_condition.
    method_name = get_token_abs( stmnt_index + 1 ).

    LOOP AT good_method_names_beginning ASSIGNING FIELD-SYMBOL(<good_name_beginning>).
      IF strlen( method_name ) GE strlen( <good_name_beginning> ) AND
         substring( val = method_name len = strlen( <good_name_beginning> ) ) EQ <good_name_beginning>.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT good_method_names_containing ASSIGNING FIELD-SYMBOL(<good_name_containing>).
      IF strlen( method_name ) GE strlen( <good_name_containing> ) AND
         method_name CS <good_name_containing>.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
       WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition
          OR stmnt_type EQ scan_struc_stmnt_type-interface.

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
    CHECK get_token_abs( statement-from ) EQ 'METHODS'.

    DATA(has_found_bool) = abap_false.
    DATA(token_index) = statement-from.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>) FROM statement-from TO statement-to.
      IF <token>-str EQ 'ABAP_BOOL' AND get_token_abs( token_index - 3 ) EQ 'RETURNING'.
        has_found_bool = abap_true.
      ENDIF.
      token_index = token_index + 1.
    ENDLOOP.

    IF has_found_bool = abap_true AND NOT contains_name_condition( statement-from ).

      DATA(check_configuration) = detect_check_configuration( statement ).
      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio
                   parameter_01        = |{ method_name }| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

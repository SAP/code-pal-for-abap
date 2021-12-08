CLASS y_pal_method_return_bool DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    DATA method_name TYPE string.

    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA good_method_names_beginning TYPE TABLE OF string.
    DATA good_method_names_containing TYPE TABLE OF string.

    METHODS contains_name_condition IMPORTING stmnt_index  TYPE i
                                    RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_PAL_METHOD_RETURN_BOOL IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC METH_RET_BOOL' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }method-return-bool.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-interface ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( 'Method &1 has a misleading name for boolean return type!' ).

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
    APPEND 'WAS_'    TO good_method_names_beginning.
    APPEND 'WERE_'   TO good_method_names_beginning.

    APPEND 'EXIST'   TO good_method_names_containing.
    APPEND 'EQUAL'   TO good_method_names_containing.
    APPEND 'CONTAIN' TO good_method_names_containing.
  ENDMETHOD.


  METHOD contains_name_condition.
    method_name = get_token_abs( stmnt_index + 1 ).

    LOOP AT good_method_names_beginning ASSIGNING FIELD-SYMBOL(<good_name_beginning>).
      IF strlen( method_name ) >= strlen( <good_name_beginning> ).
        DATA(prefix) = substring( val = method_name
                                  len = strlen( <good_name_beginning> ) ).

        IF prefix = <good_name_beginning>.
          result = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT good_method_names_containing ASSIGNING FIELD-SYMBOL(<good_name_containing>).
      IF strlen( method_name ) >= strlen( <good_name_containing> )
      AND method_name CS <good_name_containing>.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHODS'.

    DATA(has_found_bool) = abap_false.
    DATA(token_index) = statement-from.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str = 'ABAP_BOOL'
      AND get_token_abs( token_index - 3 ) = 'RETURNING'.
        has_found_bool = abap_true.
      ENDIF.
      token_index = token_index + 1.
    ENDLOOP.

    IF has_found_bool = abap_true
    AND contains_name_condition( statement-from ) = abap_false.
      DATA(check_configuration) = detect_check_configuration( statement ).

      raise_error( statement_level = statement-level
                   statement_index = index
                   statement_from = statement-from
                   check_configuration = check_configuration
                   parameter_01 = |{ method_name }| ).
    ENDIF.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

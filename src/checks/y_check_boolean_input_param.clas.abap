CLASS y_check_boolean_input_param DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_setter_method IMPORTING statement TYPE sstmnt
                             RETURNING VALUE(result) TYPE abap_bool.

    METHODS has_boolean_input_param IMPORTING statement TYPE sstmnt
                                    RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS y_check_boolean_input_param IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC BOOL_PARAM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }boolean-input-parameter.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Split method instead of Boolean input parameter!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK get_token_abs( statement-from ) = 'METHODS'.
    CHECK is_setter_method( statement ) = abap_false.
    CHECK has_boolean_input_param( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).

  ENDMETHOD.


  METHOD is_setter_method.
    DATA(method_name) = get_token_abs( statement-from + 1 ).
    result = xsdbool( method_name CS 'SET_' ).
  ENDMETHOD.


  METHOD has_boolean_input_param.
    DATA(skip) = abap_true.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.

      IF <token>-str = 'IMPORTING'.
        skip = abap_false.
      ELSEIF <token>-str = 'EXPORTING'
      OR <token>-str = 'RETURNING'
      OR <token>-str = 'CHANGING'.
        skip = abap_true.
      ENDIF.

      IF skip = abap_true.
        CONTINUE.
      ENDIF.

      IF <token>-str = 'ABAP_BOOL'.
        result = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


ENDCLASS.

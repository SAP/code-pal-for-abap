CLASS y_pal_returning_name DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS has_returning_with_wrong_name IMPORTING statement TYPE sstmnt
                                          RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_pal_returning_name IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC RET_NAME' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }returning-name.md|.

    version = version + 1.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition ) ).
    relevant_structure_types = VALUE #(  ).

    set_check_message( 'Consider calling the RETURNING parameter RESULT!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK get_token_abs( statement-from ) = 'METHODS'.
    CHECK has_returning_with_wrong_name( statement ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 check_configuration = check_configuration ).

  ENDMETHOD.


  METHOD has_returning_with_wrong_name.
    DATA(skip) = abap_true.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.

      IF <token>-str = 'RETURNING'.
        skip = abap_false.
      ENDIF.

      IF skip = abap_true.
        CONTINUE.
      ENDIF.

      IF <token>-str = 'VALUE(RESULT)'.
        RETURN.
      ELSEIF <token>-str CP 'VALUE(*)'.
        result = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

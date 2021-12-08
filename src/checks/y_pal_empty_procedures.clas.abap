CLASS y_pal_empty_procedures DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS get_next_token_from_index IMPORTING index         TYPE i
                                      RETURNING VALUE(result) TYPE stokesx.

    METHODS has_found_start_procedure IMPORTING statement     TYPE sstmnt
                                      RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_next_statement_end_proc IMPORTING statement     TYPE sstmnt
                                       RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_empty_procedures IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC EMPTY_PROCEDURE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }empty-procedure.md|.

    set_check_message( 'Empty Procedure should be removed!' ).
  ENDMETHOD.


  METHOD get_next_token_from_index.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM index WHERE type = 'I'.
      IF result IS INITIAL.
        result = <token>.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD has_found_start_procedure.
    result = abap_false.
    CASE get_next_token_from_index( statement-from )-str.
      WHEN 'FORM' OR
           'METHOD' OR
           'MODULE'.
        result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK has_found_start_procedure( statement ) = abap_true AND
          is_next_statement_end_proc( statement ) = abap_true.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = structure-stmnt_to
                 statement_from = statement-to
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD is_next_statement_end_proc.
    result = abap_false.
    CASE get_next_token_from_index( statement-to + 1 )-str.
      WHEN 'ENDFORM'
      OR 'ENDMETHOD'
      OR 'ENDMODULE'.
        result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

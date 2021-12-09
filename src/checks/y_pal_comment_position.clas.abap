CLASS y_pal_comment_position DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS has_wrong_position IMPORTING statement TYPE sstmnt
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_pseudo_comment IMPORTING token TYPE stokesx
                              RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_type_asterisk IMPORTING token TYPE stokesx
                             RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_pragma IMPORTING token TYPE stokesx
                      RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_next_token IMPORTING current_position TYPE i
                           RETURNING VALUE(result) TYPE stokesx
                           RAISING cx_sy_itab_line_not_found.

ENDCLASS.



CLASS y_pal_comment_position IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-disable_threshold_selection = abap_true.
    settings-ignore_pseudo_comments = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }comment-position.md|.

    set_check_message( 'Quote comments indent along with the statements they comment!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK statement-type = 'P'.
    CHECK has_wrong_position( statement ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD has_wrong_position.
    TRY.
        DATA(previous_token) = ref_scan->tokens[ statement-to - 1 ].
        DATA(current_token) = ref_scan->tokens[ statement-to ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF is_pseudo_comment( current_token ) = abap_true
    OR is_type_asterisk( current_token ) = abap_true.
      RETURN.
    ENDIF.

    " In-line Comment
    IF current_token-row = previous_token-row.
      result = abap_true.
      RETURN.
    ENDIF.

    TRY.
        DATA(next_token) = get_next_token( statement-to ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    " Empty Branch
    IF next_token-str CP 'END*'.
      result = COND #( WHEN next_token-str = 'ENDTRY' THEN xsdbool( current_token-row = next_token-row - 1 AND current_token-col <> next_token-col + 4 )
                                                      ELSE xsdbool( current_token-row = next_token-row - 1 AND current_token-col <> next_token-col + 2 ) ).
      RETURN.
    ENDIF.

    " Wrong Position
    IF current_token-row = next_token-row - 1
    AND current_token-col <> next_token-col.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_pseudo_comment.
    TRY.
        result = xsdbool( token-str(4) = '"#EC' ).
      CATCH cx_sy_range_out_of_bounds.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD is_type_asterisk.
    TRY.
        result = xsdbool( token-str(1) = '*' ).
      CATCH cx_sy_range_out_of_bounds.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD get_next_token.
    DATA(next_position) = current_position + 1.
    result = ref_scan->tokens[ next_position ].
    IF is_pragma( result ) = abap_true
    OR is_pseudo_comment( result ) = abap_true.
      result = get_next_token( next_position ).
    ENDIF.
  ENDMETHOD.


  METHOD is_pragma.
    TRY.
        result = xsdbool( token-str(2) = '##' ).
      CATCH cx_sy_range_out_of_bounds.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD add_check_quickfix.
    " Comments are not supported
    RETURN.
  ENDMETHOD.

ENDCLASS.

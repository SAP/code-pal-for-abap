CLASS y_check_comment_position DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    METHODS has_wrong_position IMPORTING statement TYPE sstmnt
                               RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_first_character IMPORTING token TYPE stokesx
                                RETURNING VALUE(result) TYPE char1.
    METHODS is_pseudo_comment IMPORTING token TYPE stokesx
                              RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_type_asterisk IMPORTING token TYPE stokesx
                             RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS y_check_comment_position IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }comment-position.md|.

    set_check_message( 'Quote comments indent along with the statements they comment!' ).
  ENDMETHOD.

  METHOD inspect_tokens.

    CHECK statement-type = 'P'.
    CHECK has_wrong_position( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).

  ENDMETHOD.

  METHOD has_wrong_position.
    DATA(tokens) = ref_scan_manager->get_tokens( ).

    DATA(previous_token) = tokens[ statement-to - 1 ].
    DATA(current_token) = tokens[ statement-to ].
    DATA(next_token) = tokens[ statement-to + 1 ].

    IF is_pseudo_comment( current_token ) = abap_true
    OR is_type_asterisk( current_token ) = abap_true.
      RETURN.
    ENDIF.

    " In-line Comment
    IF current_token-row = previous_token-row.
      result = abap_true.
      RETURN.
    ENDIF.

    IF next_token-str = 'ENDFORM'
    OR next_token-str = 'ENDMETHOD'
    OR next_token-str = 'ENDMODULE'.
      RETURN.
    ENDIF.

    " Wrong Position
    IF current_token-row = next_token-row - 1
    AND current_token-col <> next_token-col.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_first_character.
    TRY.
        result = token-str(1).
      CATCH cx_sy_range_out_of_bounds.
        CLEAR result.
    ENDTRY.
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

ENDCLASS.

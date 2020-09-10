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
ENDCLASS.

CLASS y_check_comment_position IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }comment-position.md|.

    set_check_message( '[Clean Code]: Quote comments indent along with the statements they comment!' ).
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
    DATA comment TYPE stokesx.
    DATA next_token TYPE stokesx.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-to.
      IF <token>-type = 'C'
      AND get_first_character( <token> ) = '"'
      AND is_pseudo_comment( <token> ) = abap_false.
        comment = <token>.
      ELSE.
        next_token = <token>.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF comment-row = next_token-row.
      result = abap_true.
    ENDIF.

    IF comment-row = next_token-row - 1
    AND comment-col <> next_token-col.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_first_character.
    result = token-str(1).
  ENDMETHOD.

  METHOD is_pseudo_comment.
    result = COND #( WHEN token-str(4) = '"#EC' THEN abap_true ).
  ENDMETHOD.

ENDCLASS.

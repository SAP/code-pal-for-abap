CLASS y_check_empty_catches DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION .

  PRIVATE SECTION.
    METHODS get_next_token_from_index IMPORTING index         TYPE i
                                      RETURNING VALUE(result) TYPE stokesx.

ENDCLASS.



CLASS Y_CHECK_EMPTY_CATCHES IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC EMPTY_CATCH' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }empty-catch.md|.

    set_check_message( 'Empty catch should be removed!' ).
  ENDMETHOD.


  METHOD get_next_token_from_index.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
      FROM index WHERE type = 'I'.
      IF result IS INITIAL.
        result = <token>.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_next_token_from_index( statement-from )-str = 'CATCH'
    AND ( get_next_token_from_index( statement-to + 1 )-str = 'ENDTRY'
          OR get_next_token_from_index( statement-to + 1 )-str = 'ENDCATCH' ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level      = statement-level
                 statement_index      = index
                 statement_from       = statement-from
                 error_priority       = check_configuration-prio ).
  ENDMETHOD.
ENDCLASS.

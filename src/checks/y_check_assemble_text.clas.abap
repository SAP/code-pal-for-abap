CLASS y_check_assemble_text DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

ENDCLASS.



CLASS y_check_assemble_text IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC ASSEMBLE_TEXT'.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }assemble-text.md|.

    set_check_message( 'Use | to assemble text!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(has_ampersand) = abap_false.
    DATA(has_literal) = abap_false.

    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str = '&&'
    OR type = scan_token_type-literal.
      IF <token>-str = '&&'.
        has_ampersand = abap_true.
      ENDIF.
      IF <token>-type = scan_token_type-literal.
        has_literal = abap_true.
      ENDIF.
    ENDLOOP.

    IF has_ampersand = abap_false
    OR has_literal = abap_false.
      RETURN.
    ENDIF.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = configuration-prio ).
  ENDMETHOD.


ENDCLASS.

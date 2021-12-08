CLASS y_pal_text_assembly DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

private section.
ENDCLASS.



CLASS Y_PAL_TEXT_ASSEMBLY IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC TEXT_ASSEMBLY'.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }text-assembly.md|.

    set_check_message( 'Use | to assemble text!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(has_ampersand) = abap_false.
    DATA(has_literal) = abap_false.
    DATA(has_identifier) = abap_false.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str = '&&'.
        has_ampersand = abap_true.
      ELSEIF <token>-type = scan_token_type-literal.
        has_literal = abap_true.
      ELSEIF <token>-type = scan_token_type-identifier
      AND sy-tabix <> statement-from.
        has_identifier = abap_true.
      ENDIF.
    ENDLOOP.

    IF has_ampersand = abap_false
    OR has_literal = abap_false
    OR has_identifier = abap_false.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.
ENDCLASS.

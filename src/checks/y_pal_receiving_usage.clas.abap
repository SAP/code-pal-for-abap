CLASS y_pal_receiving_usage DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

ENDCLASS.



CLASS y_pal_receiving_usage IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC RECEIVING_USAGE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }receiving-usage.md|.

    set_check_message( 'ABAP Keyword: "RECEIVING", should not be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) CP '*(*'.
    CHECK get_token_abs( statement-from ) <> 'BADI'.

    DATA(has_receiving) = abap_false.
    DATA(has_classic_exception) = abap_false.

    DATA(token_index) = 0.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.
      IF has_receiving = abap_false.
        has_receiving = xsdbool( <token>-str = 'RECEIVING' AND
                                 get_token_abs( statement-from + token_index + 1 ) <> '=' ).
      ENDIF.
      IF has_classic_exception = abap_false.
        has_classic_exception = xsdbool( <token>-str = 'EXCEPTIONS' AND
                                         get_token_abs( statement-from + token_index + 1 ) <> '=' ).
      ENDIF.

      token_index = token_index + 1.
    ENDLOOP.

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF has_receiving = abap_true
    AND has_classic_exception = abap_false.
      raise_error( statement_level = statement-level
                   statement_index = index
                   statement_from = statement-from
                   check_configuration = check_configuration ).
    ENDIF.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

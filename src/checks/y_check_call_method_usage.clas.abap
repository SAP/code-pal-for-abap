CLASS y_check_call_method_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

ENDCLASS.



CLASS y_check_call_method_usage IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CALL_METH_USAGE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }call-method-usage.md|.

    set_check_message( '"CALL METHOD" statement should not be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(has_keyword) = xsdbool( get_token_abs( statement-from ) = 'CALL'
                             AND get_token_abs( statement-from + 1 ) = 'METHOD' ).

    DATA(token) = get_token_abs( statement-from + 2 ).
    DATA(is_dynamic) = xsdbool( token CP '*->(*)*'
                             OR token CP '*=>(*)*'
                             OR token CP '*)=>(*)*'
                             OR token CP '*)=>*' ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS NOT INITIAL
      AND has_keyword = abap_true
      AND is_dynamic = abap_false.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.

CLASS y_check_call_method_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

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
    CHECK next1( 'CALL' ) = 'METHOD'.

    DATA(token) = next2( p_word1 = 'CALL'
                         p_word2 = 'METHOD' ).

    DATA(is_dynamic) = xsdbool( token CP '*->(*)*'
                             OR token CP '*=>(*)*'
                             OR token CP '*)=>(*)*'
                             OR token CP '*)=>*'
                             OR token CP '(*)' ).

    IF is_dynamic = abap_true.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level  = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    " There is a native feature to remove it
    RETURN.
  ENDMETHOD.

ENDCLASS.

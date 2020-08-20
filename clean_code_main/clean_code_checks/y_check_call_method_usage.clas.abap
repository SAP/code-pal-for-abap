CLASS y_check_call_method_usage DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CHECK_CALL_METHOD_USAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'CALL METHOD Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '030'.
    version = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CALL_METH_USAGE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }call-method-usage.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: "CALL METHOD" Statement should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD inspect_tokens.
    DATA(has_keyword) = xsdbool( get_token_abs( statement-from ) = 'CALL'
                             AND get_token_abs( statement-from + 1 ) = 'METHOD' ).

    DATA(token) = get_token_abs( statement-from + 2 ).
    DATA(is_dynamic) = xsdbool( token CP '*->(*)*'
                             OR token CP '*=>(*)*'
                             OR token CP '*)=>(*)*'
                             OR token CP '*)=>*' ).

    DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                            include = get_include( p_level = statement-level ) ).
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

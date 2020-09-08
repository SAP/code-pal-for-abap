CLASS y_check_not_is DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS y_check_not_is IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Prefer IS NOT to NOT IS'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '0520'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC NOT_IS' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefere-is-not-to-not-is.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Prefer IS NOT to NOT IS!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK get_token_abs( statement-from ) = 'IF'.
    CHECK get_token_abs( statement-from + 1 ) = 'NOT'.

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

CLASS y_check_equals_sign_chaining DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_EQUALS_SIGN_CHAINING' ##NO_TEXT.

    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS y_check_equals_sign_chaining IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    description = 'Equals sign chaining'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '320'.
    has_documentation = abap_false.

    settings-disable_threshold_selection = abap_true.
    settings-threshold = 1.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Values should not be allocated more than once within one statement!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD inspect_tokens.
    statement_for_message = statement.
    DATA(check_configuration) = detect_check_configuration( threshold = 1
                                                            include = get_include( p_level = statement_for_message-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF get_token_abs( statement-from + 1 ) EQ '='
      AND get_token_abs( statement-from + 3 ) EQ '='.

      raise_error( p_sub_obj_type = c_type_include
                    p_level        = statement_for_message-level
                    p_position     = index
                    p_from         = statement_for_message-from
                    p_kind         = check_configuration-prio
                    p_test         = me->myname
                    p_code         = get_code( check_configuration-prio ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

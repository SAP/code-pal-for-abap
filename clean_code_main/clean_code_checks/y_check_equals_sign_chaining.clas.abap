CLASS y_check_equals_sign_chaining DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_EQUALS_SIGN_CHAINING' ##NO_TEXT.

    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CHECK_EQUALS_SIGN_CHAINING IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    description = 'Equals sign chaining'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '320'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC EQUALS_CHAINING'.
    settings-documentation = |{ c_docs_path-checks }equals-sign-chaining.md|.
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
    DATA(check_configuration) = detect_check_configuration( threshold = 1
                                                            include = get_include( p_level = statement-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF get_token_abs( statement-from + 1 ) EQ '='
      AND get_token_abs( statement-from + 3 ) EQ '='.

      raise_error( p_sub_obj_type = c_type_include
                    p_level        = statement-level
                    p_position     = index
                    p_from         = statement-from
                    p_kind         = check_configuration-prio
                    p_test         = me->myname
                    p_code         = get_code( check_configuration-prio ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

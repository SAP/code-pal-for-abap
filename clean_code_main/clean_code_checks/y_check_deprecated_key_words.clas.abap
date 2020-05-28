CLASS y_check_deprecated_key_words DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_DEPRECATED_KEY_WORDS' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    METHODS check_if_error
      IMPORTING index   TYPE i
                keyword TYPE string.
ENDCLASS.



CLASS y_check_deprecated_key_words IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Deprecated Key Word'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '240'.
    version = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC DEPRECATED_KEY' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }deprecated-key-word.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: "&1" is deprecated'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD inspect_tokens.
    DATA(keyword) = get_token_abs( statement-from ).
    CASE keyword.
      WHEN 'MOVE' OR 'TRANSLATE'.
        statement_for_message = statement.
        check_if_error( index   = index
                        keyword = keyword ).
    ENDCASE.
  ENDMETHOD.


  METHOD check_if_error.
    DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                                include = get_include( p_level = statement_for_message-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( p_sub_obj_type = c_type_include
                 p_level        = statement_for_message-level
                 p_position     = index
                 p_from         = statement_for_message-from
                 p_kind         = check_configuration-prio
                 p_test         = me->myname
                 p_code         = get_code( check_configuration-prio )
                 p_suppress     = settings-pseudo_comment
                 p_param_1      = |{ keyword }| ).
  ENDMETHOD.
ENDCLASS.

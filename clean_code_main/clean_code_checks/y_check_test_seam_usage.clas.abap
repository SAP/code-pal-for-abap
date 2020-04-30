CLASS y_check_test_seam_usage DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_TEST_SEAM_USAGE' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CHECK_TEST_SEAM_USAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'TEST-SEAM Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '840'.
    version = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC TEST_SEAM_USAGE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: "TEST-SEAM" Statement should no longer be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) EQ 'TEST-SEAM'.

    statement_for_message = statement.

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
                 p_suppress     = settings-pseudo_comment ).
  ENDMETHOD.
ENDCLASS.

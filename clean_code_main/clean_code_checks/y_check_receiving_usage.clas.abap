CLASS y_check_receiving_usage DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_RECEIVING_USAGE' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CHECK_RECEIVING_USAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'RECEIVING Statement Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '780'.
    version = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC RECEIVING_USAGE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: ABAP Keyword: "RECEIVING", should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD inspect_tokens.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to WHERE str EQ 'RECEIVING'.

      statement_for_message = statement.

      DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                              include = get_include( p_level = statement_for_message-level ) ).
      IF check_configuration IS INITIAL.
        CONTINUE.
      ENDIF.

      raise_error( p_sub_obj_type = c_type_include
                   p_level        = statement_for_message-level
                   p_position     = index
                   p_from         = statement_for_message-from
                   p_kind         = check_configuration-prio
                   p_test         = me->myname
                   p_code         = get_code( check_configuration-prio )
                   p_suppress     = settings-pseudo_comment ).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS y_check_sub_assign_read_table DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_SUB_ASSIGN_READ_TABLE' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA token_index TYPE i VALUE 0.
    DATA into_index TYPE i VALUE 0.
ENDCLASS.



CLASS Y_CHECK_SUB_ASSIGN_READ_TABLE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'READ TABLE with subsequent memory assignment'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '810'.
    version = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC SUB_ASSIGN' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: "READ TABLE" with subsequent memory assignment should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) EQ 'READ' AND
          get_token_abs( statement-from + 1 ) EQ 'TABLE'.

    token_index = statement-from.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
       FROM statement-from TO statement-to.

      IF <token>-str EQ 'INTO'.
        into_index = token_index + 1.

      ELSEIF into_index EQ token_index AND
             <token>-str CS '<' AND
             <token>-str CS '>'.

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
      ENDIF.

      token_index = token_index + 1.

    ENDLOOP.

    into_index = 0.
    token_index = 0.
  ENDMETHOD.
ENDCLASS.

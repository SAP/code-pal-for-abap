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
    settings-documentation = |{ c_docs_path-checks }sub-assign-read-table.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: "READ TABLE" with subsequent memory assignment should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD inspect_tokens.

    DATA fieldname TYPE string VALUE IS INITIAL.

    CHECK get_token_abs( statement-from ) = 'READ' AND
          get_token_abs( statement-from + 1 ) = 'TABLE'.

    " First Read Table
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str CP 'FIELD-SYMBOL(<*>)'.
        fieldname = <token>-str.
        REPLACE 'FIELD-SYMBOL(' IN fieldname WITH ''.
        REPLACE ')' IN fieldname WITH ''.
      ENDIF.
    ENDLOOP.

    IF fieldname IS INITIAL.
      RETURN.
    ENDIF.

    " Second Read Table
    DATA(position) = index + 1.

    LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to.

      IF statement-to > <statement>-to.
        CONTINUE.
      ENDIF.

      IF get_token_abs( <statement>-from ) <> 'READ'
      AND get_token_abs( <statement>-from + 1 ) <> 'TABLE'.

        position = position + 1.
        CONTINUE.
      ENDIF.

      LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING <token>
      FROM <statement>-from TO <statement>-to.

        IF <token>-str <> fieldname.
          CONTINUE.
        ENDIF.

        DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                                include = get_include( p_level = <statement>-level ) ).
        IF check_configuration IS INITIAL.
          CONTINUE.
        ENDIF.

        raise_error( p_sub_obj_type = c_type_include
                     p_level        = <statement>-level
                     p_position     = position
                     p_from         = <statement>-from
                     p_kind         = check_configuration-prio
                     p_test         = me->myname
                     p_code         = get_code( check_configuration-prio ) ).


      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

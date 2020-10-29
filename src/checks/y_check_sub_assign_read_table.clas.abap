CLASS y_check_sub_assign_read_table DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    METHODS is_read_table IMPORTING statement     TYPE sstmnt
                          RETURNING VALUE(result) TYPE abap_bool.
    METHODS extract_fieldname IMPORTING statement     TYPE sstmnt
                              RETURNING VALUE(result) TYPE string.
    METHODS might_cause_undesired_changes IMPORTING statement     TYPE sstmnt
                                                    fieldname     TYPE string
                                          RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_CHECK_SUB_ASSIGN_READ_TABLE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC SUB_ASSIGN' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }sub-assign-read-table.md|.

    set_check_message( 'Subsequent memory assign to the Read Table might cause undesired changes!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK is_read_table( statement ) = abap_true.

    DATA(fieldname) = extract_fieldname( statement ).

    IF fieldname IS INITIAL.
      RETURN.
    ENDIF.

    DATA(position) = index.

    LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
    FROM index TO structure-stmnt_to.

      IF is_read_table( <statement> ) = abap_false.
        position = position + 1.
        CONTINUE.
      ENDIF.

      DATA(might_cause_undesired_changes) = might_cause_undesired_changes( statement = <statement>
                                                                           fieldname = fieldname ).

      IF might_cause_undesired_changes = abap_false.
        position = position + 1.
        CONTINUE.
      ENDIF.

      DATA(check_configuration) = detect_check_configuration( <statement> ).

      IF check_configuration IS INITIAL.
        CONTINUE.
      ENDIF.

      raise_error( statement_level     = <statement>-level
                   statement_index     = position
                   statement_from      = <statement>-from
                   error_priority      = check_configuration-prio ).

    ENDLOOP.
  ENDMETHOD.


  METHOD is_read_table.
    CHECK get_token_abs( statement-from ) = 'READ'.
    CHECK get_token_abs( statement-from + 1 ) = 'TABLE'.
    result = abap_true.
  ENDMETHOD.


  METHOD extract_fieldname.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str CP 'FIELD-SYMBOL(<*>)'.
      result = <token>-str.
      REPLACE 'FIELD-SYMBOL(' IN result WITH ''.
      REPLACE ')' IN result WITH ''.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD might_cause_undesired_changes. "#EC METH_RET_BOOL
    DATA(tokens) = ref_scan_manager->get_tokens( ).
    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str <> 'INTO'.
        CONTINUE.
      ENDIF.
      DATA(target) = tokens[ sy-tabix + 1 ].
      IF target-str = fieldname.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

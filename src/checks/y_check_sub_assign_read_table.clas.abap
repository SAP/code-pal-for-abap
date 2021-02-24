CLASS y_check_sub_assign_read_table DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_read_table IMPORTING statement     TYPE sstmnt
                          RETURNING VALUE(result) TYPE abap_bool.

    METHODS extract_fieldname IMPORTING statement     TYPE sstmnt
                              RETURNING VALUE(result) TYPE string.

    METHODS has_subsequent_read IMPORTING statement     TYPE sstmnt
                                          fieldname     TYPE string
                                RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS y_check_sub_assign_read_table IMPLEMENTATION.


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

    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM index TO structure-stmnt_to.

      IF is_read_table( <statement> ) = abap_false.
        position = position + 1.
        CONTINUE.
      ENDIF.

      DATA(has_subsequent_read) = has_subsequent_read( statement = <statement>
                                                       fieldname = fieldname ).

      IF has_subsequent_read = abap_false.
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
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str CP 'FIELD-SYMBOL(<*>)'.
      result = <token>-str.
      REPLACE 'FIELD-SYMBOL(' IN result WITH ''.
      REPLACE ')' IN result WITH ''.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD has_subsequent_read.
    DATA(tokens) = ref_scan_manager->tokens.
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

CLASS y_check_empty_procedures DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_EMPTY_PROCEDURES' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS inspect_tokens REDEFINITION .
  PRIVATE SECTION.
    DATA is_empty TYPE abap_bool VALUE abap_false.

    METHODS is_statement_type_excluded
      IMPORTING statement     TYPE sstmnt
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_next_token_from_index
      IMPORTING index         TYPE i
      RETURNING VALUE(result) TYPE stokesx.

    METHODS has_found_start_procedure
      IMPORTING statement     TYPE sstmnt
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_next_statement_end_proc
      IMPORTING statement     TYPE sstmnt
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_CHECK_EMPTY_PROCEDURES IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Empty Procedure'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '300'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC EMPTY_PROCEDURE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Empty Procedure should be removed!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD get_next_token_from_index.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM index WHERE type EQ 'I'.
      IF result IS INITIAL.
        result = <token>.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD has_found_start_procedure.
    result = abap_false.
    CASE get_next_token_from_index( statement-from )-str.
      WHEN  if_kaizen_keywords_c=>gc_form OR
            if_kaizen_keywords_c=>gc_method OR
            if_kaizen_keywords_c=>gc_function OR
            if_kaizen_keywords_c=>gc_module.
        result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK is_statement_type_excluded( statement ) = abap_false.

    IF is_empty = abap_true AND structure-stmnt_to EQ index.
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

      is_empty = abap_false.
    ENDIF.

    IF has_found_start_procedure( statement ) = abap_true AND
       is_next_statement_end_proc( statement ) = abap_true.
      is_empty = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_next_statement_end_proc.
    result = abap_false.
    CASE get_next_token_from_index( statement-to + 1 )-str.
      WHEN  if_kaizen_keywords_c=>gc_endform OR
            if_kaizen_keywords_c=>gc_endmethod OR
            if_kaizen_keywords_c=>gc_endfunction OR
            if_kaizen_keywords_c=>gc_endmodule.
        result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD is_statement_type_excluded.
    result = xsdbool( statement-type EQ scan_stmnt_type-empty OR
                      statement-type EQ scan_stmnt_type-comment OR
                      statement-type EQ scan_stmnt_type-comment_in_stmnt OR
                      statement-type EQ scan_stmnt_type-pragma ).
  ENDMETHOD.
ENDCLASS.

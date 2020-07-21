class Y_CHECK_EMPTY_CATCHES definition
  public
  inheriting from Y_CHECK_BASE
  create public .

public section.

  constants C_MYNAME type SCI_CHK value 'Y_CHECK_EMPTY_CATCHES' ##NO_TEXT.

  methods CONSTRUCTOR .
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

ENDCLASS.



CLASS Y_CHECK_EMPTY_CATCHES IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Empty CATCH'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '300'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC EMPTY_CATCH' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-prio = 'W'.
    settings-documentation = |{ c_docs_path-checks }empty-catch.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Empty catch should be removed!'(102)
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


  METHOD inspect_tokens.
    CHECK is_statement_type_excluded( statement ) = abap_false AND
          get_next_token_from_index( statement-from )-str EQ 'CATCH' AND
        ( get_next_token_from_index( statement-to + 1 )-str EQ 'ENDTRY' OR
          get_next_token_from_index( statement-to + 1 )-str EQ 'ENDCATCH' ).

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
                 p_code         = get_code( check_configuration-prio ) ).
  ENDMETHOD.


  METHOD is_statement_type_excluded.
    result = xsdbool( statement-type EQ scan_stmnt_type-empty OR
                      statement-type EQ scan_stmnt_type-comment OR
                      statement-type EQ scan_stmnt_type-comment_in_stmnt OR
                      statement-type EQ scan_stmnt_type-pragma ).
  ENDMETHOD.
ENDCLASS.

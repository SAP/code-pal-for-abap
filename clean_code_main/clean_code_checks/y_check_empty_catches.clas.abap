CLASS y_check_empty_catches DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS inspect_tokens REDEFINITION .
  PRIVATE SECTION.
    DATA is_empty TYPE abap_bool VALUE abap_false.

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
    CHECK get_next_token_from_index( statement-from )-str EQ 'CATCH' AND
        ( get_next_token_from_index( statement-to + 1 )-str EQ 'ENDTRY' OR
          get_next_token_from_index( statement-to + 1 )-str EQ 'ENDCATCH' ).

    DATA(check_configuration) = detect_check_configuration( error_count = 0
                                                            include = get_include( p_level = statement-level ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level      = statement-level
                 statement_index      = index
                 statement_from       = statement-from
                 error_priority       = check_configuration-prio ).
  ENDMETHOD.
ENDCLASS.

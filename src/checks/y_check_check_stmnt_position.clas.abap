CLASS y_check_check_stmnt_position DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_not_relevant_token IMPORTING token         TYPE string
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS has_wrong_position IMPORTING structure     TYPE sstruc
                                         check         TYPE sstmnt
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_check_in_loop IMPORTING structure     TYPE sstruc
                             RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_check_check_stmnt_position IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CHECK_POSITION' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }check-statement-position.md|.

    set_check_message( 'Do not use CHECK outside of the initialization section!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'CHECK'.

    CHECK has_wrong_position( structure = structure
                              check = statement ) = abap_true.

    CHECK is_check_in_loop( structure ) = abap_false.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD is_not_relevant_token.
    result = xsdbool(    token = 'METHOD'
                      OR token = 'FORM'
                      OR token = 'FUNCTION'
                      OR token = 'MODULE'
                      OR token = 'DATA'
                      OR token = 'TYPES'
                      OR token = 'CHECK'
                      OR token = 'FIELD-SYMBOLS'
                      OR token = 'CONSTANTS' ).
  ENDMETHOD.


  METHOD has_wrong_position.
    LOOP AT ref_scan->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to.
      IF <statement>-type = scan_stmnt_type-empty
      OR <statement>-type = scan_stmnt_type-comment
      OR <statement>-type = scan_stmnt_type-comment_in_stmnt.
        CONTINUE.
      ENDIF.

      IF <statement>-number = check-number.
        RETURN.
      ENDIF.

      IF is_not_relevant_token( get_token_abs( <statement>-from ) ) = abap_false.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_check_in_loop.
    DATA(current) = structure.

    DO.
      DATA(scope) = ref_scan->statements[ current-stmnt_from ].
      DATA(token) = get_token_abs( scope-from ).
      IF token = 'LOOP' OR token = 'DO' OR token = 'WHILE'.
        result = abap_true.
        RETURN.
      ENDIF.

      IF current-back <= 0.
        RETURN.
      ENDIF.
      current = ref_scan->structures[ current-back ].
    ENDDO.
  ENDMETHOD.

ENDCLASS.

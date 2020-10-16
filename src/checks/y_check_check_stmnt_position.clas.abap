CLASS y_check_check_stmnt_position DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS execute_check REDEFINITION.

  PRIVATE SECTION.

    DATA statement_index TYPE i VALUE 0 ##NO_TEXT.

    METHODS is_token_excluded
      IMPORTING
        !token_str    TYPE stokesx-str
      RETURNING
        VALUE(result) TYPE abap_bool .
ENDCLASS.



CLASS Y_CHECK_CHECK_STMNT_POSITION IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CHECK_POSITION' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }check-statement-position.md|.

    set_check_message( '"CHECK" statement should be the very first statement!' ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-form
         OR stmnt_type EQ scan_struc_stmnt_type-method
         OR stmnt_type EQ scan_struc_stmnt_type-function
         OR stmnt_type EQ scan_struc_stmnt_type-module
         OR type EQ scan_struc_type-event.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      DATA(index) = <structure>-stmnt_from.
      statement_index = 0.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( index = index
                        statement = <statement> ).

        index = index + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-from
      WHERE type NE scan_token_type-comment AND
            type NE scan_token_type-pragma.
      IF is_token_excluded( <token>-str ) EQ abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    statement_index = statement_index + 1.

    IF statement_index GT 1 AND get_token_abs( statement-from ) EQ 'CHECK'.

      DATA(check_configuration) = detect_check_configuration( statement ).

      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio ).
    ENDIF.
  ENDMETHOD.


  METHOD is_token_excluded.
    result = xsdbool( token_str EQ 'METHOD' OR
                      token_str EQ 'FORM' OR
                      token_str EQ 'FUNCTION' OR
                      token_str EQ 'MODULE' OR
                      token_str EQ 'DATA' OR
                      token_str EQ 'TYPES' OR
                      token_str CP 'DATA(*)' OR
                      ( token_str EQ 'CHECK' AND statement_index EQ 0 ) ).
  ENDMETHOD.
ENDCLASS.

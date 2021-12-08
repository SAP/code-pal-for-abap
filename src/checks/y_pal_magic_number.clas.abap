CLASS y_pal_magic_number DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS is_magic_number RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_index RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_lines RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_sy_in_case RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_magic_number IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_MAGIC' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_false.
    settings-documentation = |{ c_docs_path-checks }magic-number.md|.

    set_check_message( 'Magic Number Violation - &1 is a Magic Number!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(keyword) = keyword( ).

    IF keyword <> if_kaizen_keywords_c=>gc_do
    AND keyword <> if_kaizen_keywords_c=>gc_if
    AND keyword <> if_kaizen_keywords_c=>gc_elseif
    AND keyword <> if_kaizen_keywords_c=>gc_when
    AND keyword <> if_kaizen_keywords_c=>gc_check.
      RETURN.
    ENDIF.

    IF keyword = if_kaizen_keywords_c=>gc_when
    AND is_sy_in_case( ) = abap_true.
      RETURN.
    ELSEIF next1( 'SY-SUBRC' ) IS NOT INITIAL
    OR next1( 'SY-TABIX' ) IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ref_scan->tokens INTO token_wa
    FROM statement-from TO statement-to.
      IF is_magic_number( ) = abap_false
      OR is_lines( ) = abap_true
      OR is_index( ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(check_configuration) = detect_check_configuration( statement ).

      raise_error( statement_level = statement-level
                   statement_index = index
                   statement_from = statement-from
                   check_configuration = check_configuration
                   parameter_01 = |{ token_wa-str }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD is_magic_number.
    DATA(string) = token_wa-str.
    REPLACE ALL OCCURRENCES OF |'| IN string WITH ||.

    result = xsdbool( string IS NOT INITIAL
                  AND string CO '0123456789'
                  AND string CN '0'
                  AND string <> '1'
                  AND string <> '0123456789' ).
  ENDMETHOD.


  METHOD is_index.
    DATA(current) = line_index( ref_scan->tokens[ table_line = token_wa ] ).
    DATA(last) = VALUE #( ref_scan->tokens[ current - 1 ] OPTIONAL ).
    DATA(next) = VALUE #( ref_scan->tokens[ current + 1 ] OPTIONAL ).

    result = xsdbool( last-str CA '[('
                   OR next-str CA '])' ).
  ENDMETHOD.


  METHOD is_lines.
    result = xsdbool( next1( 'LINES(' ) ).
  ENDMETHOD.


  METHOD is_sy_in_case.
    DATA(when_statement) = statement_wa.
    DATA(when_structure) = ref_scan->structures[ when_statement-struc ].

    DATA(case_structure) = ref_scan->structures[ when_structure-back ].
    DATA(case_statement) = ref_scan->statements[ case_structure-stmnt_from ].
    DATA(case_target) = ref_scan->tokens[ case_statement-to ].

    result = xsdbool( case_target-str = 'SY-SUBRC'
                   OR case_target-str = 'SY-TABIX' ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

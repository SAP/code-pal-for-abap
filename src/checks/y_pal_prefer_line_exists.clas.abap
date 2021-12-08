CLASS y_pal_prefer_line_exists DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

ENDCLASS.



CLASS y_pal_prefer_line_exists IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREF_LINE_EX' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-line-exists.md|.

    set_check_message( 'Prefer LINE_EXISTS or LINE_INDEX to READ TABLE or LOOP AT!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(keyword) = keyword( ).

    IF keyword <> if_kaizen_keywords_c=>gc_loop
    AND keyword <> 'READ'.
      RETURN.
    ENDIF.

    DATA(transporting_no_fields) = next2( p_word1 = 'TRANSPORTING'
                                          p_word2 = 'NO' ).

    IF transporting_no_fields IS INITIAL.
      RETURN.
    ENDIF.

    IF next1( 'FROM' ) IS NOT INITIAL
    OR next1( 'TO' ) IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF next1( 'IN' ) IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

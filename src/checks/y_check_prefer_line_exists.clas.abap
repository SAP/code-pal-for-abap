CLASS y_check_prefer_line_exists DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      get_valid_last_token  IMPORTING last_token              TYPE stokesx
                            RETURNING VALUE(valid_last_token) TYPE abap_boolean,
      get_valid_first_token IMPORTING first_token              TYPE string
                            RETURNING VALUE(valid_first_token) TYPE abap_boolean.

ENDCLASS.



CLASS y_check_prefer_line_exists IMPLEMENTATION.


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

    IF keyword = 'LOOP'.
      DATA(loop_structure) = ref_scan->structures[ stmnt_from = index type = scan_struc_type-iteration ].
      LOOP AT ref_scan->statements ASSIGNING FIELD-SYMBOL(<statement>) FROM loop_structure-stmnt_from TO loop_structure-stmnt_to.
        DATA(first_token) = ref_scan->tokens[ <statement>-from ]-str.
        IF NOT get_valid_first_token( first_token ).
          DATA(last_token) = ref_scan->tokens[ <statement>-to ].
          IF get_valid_last_token( last_token ).
            RETURN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DATA(line) = get_line_abs( statement-from ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.

  METHOD get_valid_first_token.
    valid_first_token = xsdbool( first_token = 'LOOP' OR first_token = 'ENDLOOP' OR first_token = 'EXIT' ).
  ENDMETHOD.

  METHOD get_valid_last_token.
    valid_last_token = xsdbool( NOT ( last_token-type = scan_token_type-comment OR last_token-type = scan_token_type-pragma ) AND NOT ( last_token-str = 'ABAP_TRUE' OR last_token-str = 'ABAP_FALSE' ) ).
  ENDMETHOD.

ENDCLASS.

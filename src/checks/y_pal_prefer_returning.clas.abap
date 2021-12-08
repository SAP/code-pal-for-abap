CLASS y_pal_prefer_returning DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS has_only_one_exporting IMPORTING statement TYPE sstmnt
                                   RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_exception_case IMPORTING position TYPE sy-tabix
                              RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_prefer_returning IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREFER_RET' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-returning-to-exporting.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-interface ) ).

    set_check_message( 'Prefer RETURNING to EXPORTING!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHODS'
    OR get_token_abs( statement-from ) = 'CLASS-METHODS'.

    CHECK has_only_one_exporting( statement ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD has_only_one_exporting.
    DATA(skip) = abap_false.
    DATA(count) = 0.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from
    TO statement-to.

      IF <token>-str = 'IMPORTING'
      OR <token>-str = 'CHANGING'
      OR <token>-str = 'RETURNING'
      OR <token>-str = 'RAISING'.
        skip = abap_true.
        CLEAR count.
      ELSEIF <token>-str = 'EXPORTING'.
        skip = abap_false.
      ENDIF.

      IF skip = abap_true.
        CONTINUE.
      ENDIF.

      DATA(is_declaration) = xsdbool(    <token>-str = 'TYPE'
                                      OR <token>-str = 'LIKE' ).

      IF is_declaration = abap_true
      AND is_exception_case( sy-tabix ) = abap_false.
        count = count + 1.
      ENDIF.

    ENDLOOP.

    result = xsdbool( count = 1 ).
  ENDMETHOD.


  METHOD is_exception_case.
    TRY.
        DATA(one_ahead) = ref_scan->tokens[ position + 1 ]-str.
        DATA(two_ahead) = ref_scan->tokens[ position + 2 ]-str.

        result = xsdbool(     one_ahead = 'STANDARD'
                          AND two_ahead = 'TABLE' ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

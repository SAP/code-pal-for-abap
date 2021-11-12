CLASS y_check_omit_optional_exp DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS has_optional_exporting RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_check_omit_optional_exp IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC OPTL_EXP' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }omit-optional-exporting.md|.

    set_check_message( 'Omit the optional keyword EXPORTING!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK statement-type = scan_stmnt_type-method_direct
       OR statement-type = scan_stmnt_type-compute_direct.

    CHECK has_optional_exporting( ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 check_configuration  = check_configuration ).
  ENDMETHOD.


  METHOD has_optional_exporting.
    LOOP AT ref_scan->tokens INTO token_wa
    FROM statement_wa-from TO statement_wa-to.
      IF token_wa-str = 'EXPORTING'.
        result = abap_true.
      ELSEIF token_wa-str = 'IMPORTING'
      OR token_wa-str = 'CHANGING'
      OR token_wa-str = 'RECEIVING'
      OR token_wa-str = 'EXCEPTIONS'.
        result = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_check_quickfix.
    TRY.
        new_quickfix( )->replace_by( p_new_code = ``
                                     p_context = cl_ci_quickfix_abap_context=>create_from_scan_tokens( p_ci_scan = ref_scan
                                                                                                       p_stmt_idx = statement_index
                                                                                                       p_from_token = 2 ) ).
      CATCH cx_ci_quickfix_failed.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS y_pal_cx_root_usage DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS has_cx_root IMPORTING statement     TYPE sstmnt
                        RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_cx_root_usage IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NEED_CX_ROOT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }cx-root-usage.md|.

    set_check_message( 'CX_ROOT should not be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'CATCH'.
    CHECK has_cx_root( statement ) = abap_true.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD has_cx_root.
    LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS
    FROM statement-from TO statement-to
    WHERE str = 'CX_ROOT'.
      result = abap_true.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

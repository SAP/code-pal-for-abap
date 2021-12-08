CLASS y_pal_empty_catches DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS get_token_abs REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS is_test_double_framework RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_empty_catches IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC EMPTY_CATCH'.
    settings-alternative_pseudo_comment = '"#EC NO_HANDLER'.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }empty-catch.md|.

    set_check_message( 'Empty catch should be removed!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK keyword( ) = 'CATCH'.

    CHECK get_token_abs( statement-to + 1 ) = if_kaizen_keywords_c=>gc_endtry
       OR get_token_abs( statement-to + 1 ) = 'ENDCATCH'.

    CHECK is_test_double_framework( ) = abap_false.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD get_token_abs.
    p_result = super->get_token_abs( p_n ).
    if token_wa-type = scan_token_type-comment.
      p_result = get_token_abs( p_n + 1 ).
    endif.
  ENDMETHOD.


  METHOD is_test_double_framework.
    DATA(catch_structure) = ref_scan->structures[ statement_wa-struc ].
    DATA(before_try_structure) = ref_scan->structures[ catch_structure-back - 1 ].
    DATA(search_from_token) = ref_scan->statements[ before_try_structure-stmnt_from ]-from.
    DATA(search_to_token) = ref_scan->statements[ before_try_structure-stmnt_to ]-to.
    DATA(range_tokens) = VALUE stokesx_tab( FOR token IN ref_scan->tokens FROM search_from_token TO search_to_token ( token ) ).
    result = xsdbool( line_exists( range_tokens[ str = 'CL_ABAP_TESTDOUBLE=>CONFIGURE_CALL(' ] ) ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

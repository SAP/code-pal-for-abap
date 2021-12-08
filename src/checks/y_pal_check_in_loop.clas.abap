CLASS y_pal_check_in_loop DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS get_back_statement IMPORTING structure     TYPE sstruc
                               RETURNING VALUE(result) TYPE sstmnt.

ENDCLASS.



CLASS y_pal_check_in_loop IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CHECK_IN_LOOP' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }check-in-loop.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-check ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Use an IF statement in combination with CONTINUE instead CHECK!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'CHECK'.
    CHECK get_token_abs( get_back_statement( structure )-from ) = 'LOOP'.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD get_back_statement.
    TRY.
        DATA(back_structure) = ref_scan->structures[ structure-back ].
        result = ref_scan->statements[ back_structure-stmnt_from ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

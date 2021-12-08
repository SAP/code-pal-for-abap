CLASS y_pal_constants_interface DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA has_something_else TYPE abap_bool VALUE abap_false.

    METHODS is_structure_empty IMPORTING structure TYPE sstruc
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS check_result IMPORTING structure TYPE sstruc.

ENDCLASS.



CLASS y_pal_constants_interface IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CONS_INTF' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }constants-interface.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-interface ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'There are only constants in this interface!' ).
  ENDMETHOD.


  METHOD inspect_statements.
    CHECK is_structure_empty( structure ) = abap_false.

    has_something_else = abap_false.

    super->inspect_statements( structure ).

    check_result( structure ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK statement-type <> scan_stmnt_type-comment.
    CHECK statement-type <> scan_stmnt_type-comment_in_stmnt.
    CHECK statement-type <> scan_stmnt_type-pragma.

    DATA(token) = get_token_abs( statement-from ).

    IF token <> 'CONSTANTS'
    AND token <> 'INTERFACE'
    AND token <> 'ENDINTERFACE'
    AND token <> 'BEGIN'
    AND token <> 'END'
    AND token <> 'OF'.
      has_something_else = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD check_result.
    CHECK has_something_else = abap_false.

    DATA(statement_for_message) = ref_scan->statements[ structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( statement_for_message ).

    raise_error( statement_level = statement_for_message-level
                 statement_index = structure-stmnt_from
                 statement_from = statement_for_message-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD is_structure_empty.
    result = xsdbool( structure-stmnt_from = structure-stmnt_to - 1 ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

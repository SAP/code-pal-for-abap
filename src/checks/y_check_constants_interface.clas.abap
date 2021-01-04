CLASS y_check_constants_interface DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA leading_structure TYPE sstruc.
    DATA has_something_else TYPE abap_bool VALUE abap_false.

    METHODS check_leading_structure.

    METHODS is_structure_empty IMPORTING structure TYPE sstruc
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS set_leading_structure IMPORTING structure TYPE sstruc.

ENDCLASS.



CLASS Y_CHECK_CONSTANTS_INTERFACE IMPLEMENTATION.


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


  METHOD execute_check.
    super->execute_check( ).
    check_leading_structure( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK is_structure_empty( structure ) = abap_false.

    CHECK statement-type <> scan_stmnt_type-comment.
    CHECK statement-type <> scan_stmnt_type-comment_in_stmnt.
    CHECK statement-type <> scan_stmnt_type-pragma.

    IF leading_structure <> structure.
      check_leading_structure( ).
      set_leading_structure( structure ).
    ENDIF.

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


  METHOD check_leading_structure.
    CHECK leading_structure IS NOT INITIAL.

    DATA(statement_for_message) = ref_scan_manager->statements[ leading_structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( statement_for_message ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF has_something_else EQ abap_false.
      raise_error( statement_level     = statement_for_message-level
                   statement_index     = leading_structure-stmnt_from
                   statement_from      = statement_for_message-from
                   error_priority      = check_configuration-prio ).
    ENDIF.
  ENDMETHOD.


  METHOD is_structure_empty.
    result = xsdbool( structure-stmnt_from = structure-stmnt_to - 1 ).
  ENDMETHOD.


  METHOD set_leading_structure.
    leading_structure = structure.
    has_something_else = abap_false.
  ENDMETHOD.


ENDCLASS.

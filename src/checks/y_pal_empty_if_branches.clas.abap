CLASS y_pal_empty_if_branches DEFINITION
  PUBLIC
  INHERITING FROM y_code_pal_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA branch_counter TYPE i VALUE 0.
    DATA found_statement TYPE abap_bool VALUE abap_false.
    CONSTANTS first_if TYPE i VALUE 1.

    METHODS set_found_statement_to_true.
    METHODS set_found_statement_to_false.

    METHODS begin_of_statement
      IMPORTING statement TYPE sstmnt.

    METHODS branch_search_in_next_stmnt
      IMPORTING index     TYPE i
                statement TYPE sstmnt.

    METHODS get_first_token_from_index
      IMPORTING index         TYPE i
      RETURNING VALUE(result) TYPE stokesx.

    METHODS is_statement_type_excluded
      IMPORTING statement     TYPE sstmnt
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS y_pal_empty_if_branches IMPLEMENTATION.


  METHOD begin_of_statement.
    CASE get_first_token_from_index( statement-from )-str.
      WHEN 'IF'.
        branch_counter = branch_counter + 1.

      WHEN 'ENDIF'.
        branch_counter = branch_counter - 1.

      WHEN 'ELSEIF' OR 'ELSE'.
        found_statement = abap_false.

      WHEN OTHERS.
        found_statement = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD branch_search_in_next_stmnt.
    CHECK branch_counter = first_if AND found_statement = abap_false.

    CASE get_first_token_from_index( statement-to + 1 )-str.
      WHEN 'ELSEIF'
        OR 'ELSE'
        OR 'ENDIF'.

        DATA(check_configuration) = detect_check_configuration( statement_wa ).

        raise_error( statement_level = statement_wa-level
                     statement_index = index
                     statement_from = statement_wa-from
                     check_configuration  = check_configuration ).
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC EMPTY_IF_BRANCH' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }empty-if-branches.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-if ) ).
    relevant_structure_types = VALUE #( (  ) ).

    set_check_message( 'Empty IF-Branch should be removed!' ).
  ENDMETHOD.


  METHOD get_first_token_from_index.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM index
    WHERE type = 'I'.
      IF result IS INITIAL.
        result = <token>.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK is_statement_type_excluded( statement ) = abap_false.
    statement_wa = statement.
    begin_of_statement( statement ).

    set_found_statement_to_true( ).
    branch_search_in_next_stmnt( index = index
                                 statement = statement ).
    set_found_statement_to_false( ).
  ENDMETHOD.


  METHOD is_statement_type_excluded.
    result = xsdbool( statement-type = scan_stmnt_type-empty OR
                      statement-type = scan_stmnt_type-comment OR
                      statement-type = scan_stmnt_type-comment_in_stmnt OR
                      statement-type = scan_stmnt_type-pragma ).
  ENDMETHOD.


  METHOD set_found_statement_to_false.
    IF branch_counter < first_if.
      found_statement = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD set_found_statement_to_true.
    IF branch_counter > first_if.
      found_statement = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

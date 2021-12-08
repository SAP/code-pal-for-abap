CLASS y_pal_prefer_case_to_elseif DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_structures REDEFINITION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_counter,
             if_structure TYPE sstruc,
             if_statement TYPE sstmnt,
             condition    TYPE string,
             count        TYPE i,
           END OF ty_counter.

    TYPES ty_counters TYPE TABLE OF ty_counter.

    DATA counters TYPE ty_counters.

    METHODS has_multiple_conditions IMPORTING statement     TYPE sstmnt
                                    RETURNING VALUE(result) TYPE abap_bool.

    METHODS check_result.

ENDCLASS.



CLASS y_pal_prefer_case_to_elseif IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREFER_CASE' ##NO_TEXT.
    settings-threshold = 5.
    settings-documentation = |{ c_docs_path-checks }prefer-case-to-elseif.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-if )
                                        ( scan_struc_stmnt_type-elseif ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( 'Prefer CASE to ELSE IF for multiple alternative conditions!' ).
  ENDMETHOD.


  METHOD inspect_structures.
    CLEAR counters.

    super->inspect_structures( ).

    check_result( ).
  ENDMETHOD.


  METHOD inspect_statements.
    DATA(if_statement) = ref_scan->statements[ structure-stmnt_from ].

    IF has_multiple_conditions( if_statement ) = abap_true.
      RETURN.
    ENDIF.

    DATA(if_structure) = COND #( WHEN structure-stmnt_type = scan_struc_stmnt_type-if THEN structure
                                 WHEN structure-stmnt_type = scan_struc_stmnt_type-elseif THEN ref_scan->structures[ structure-back ] ).

    IF if_structure IS INITIAL.
      RETURN.
    ENDIF.

    DATA(condition) = get_token_abs( if_statement-from + 1 ).

    TRY.
        counters[ if_structure = if_structure
                  condition = condition ]-count = counters[ if_structure = if_structure
                                                            condition = condition ]-count + 1.
      CATCH cx_sy_itab_line_not_found.
        counters = VALUE #( BASE counters
                          ( if_structure = if_structure
                            if_statement = if_statement
                            condition = condition
                            count = 1 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD check_result.
    LOOP AT counters ASSIGNING FIELD-SYMBOL(<counter>).
      DATA(check_configuration) = detect_check_configuration( error_count = <counter>-count
                                                              statement = <counter>-if_statement ).

      raise_error( statement_level = <counter>-if_statement-level
                   statement_index = <counter>-if_structure-stmnt_from
                   statement_from = <counter>-if_statement-from
                   check_configuration = check_configuration ).
    ENDLOOP.
  ENDMETHOD.


  METHOD has_multiple_conditions.
    LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS
    FROM statement-from TO statement-to
    WHERE str = 'AND' OR str = 'OR'.
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

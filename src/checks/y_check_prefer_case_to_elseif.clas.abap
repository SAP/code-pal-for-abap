CLASS y_check_prefer_case_to_elseif DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF counter,
             if_structure TYPE sstruc,
             if_statement TYPE sstmnt,
             condition    TYPE string,
             count        TYPE i,
           END OF counter.
    TYPES counters TYPE TABLE OF counter.
    TYPES: helper_type TYPE sstmnt.
    METHODS has_multiple_conditions
      IMPORTING
        statement     TYPE helper_type
      RETURNING
        value(result) TYPE abap_bool.

    METHODS should_skip_test_code IMPORTING structure     TYPE sstruc
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS handle_result IMPORTING counters TYPE counters.

ENDCLASS.



CLASS y_check_prefer_case_to_elseif IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREFER_CASE' ##NO_TEXT.
    settings-threshold = 5.
    settings-documentation = |{ c_docs_path-checks }prefer-case-to-elseif.md|.

    set_check_message( 'Prefer CASE to ELSE IF for multiple alternative conditions!' ).
  ENDMETHOD.


  METHOD execute_check.
    DATA counters TYPE counters.

    DATA(structures) = ref_scan_manager->get_structures( ).
    DATA(statements) = ref_scan_manager->get_statements( ).

    LOOP AT structures ASSIGNING FIELD-SYMBOL(<structure>)
    WHERE type = scan_struc_type-condition
    OR type = scan_struc_type-alternation.

      IF should_skip_test_code( <structure> ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(statement) = statements[ <structure>-stmnt_from ].

      IF has_multiple_conditions( statement ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(token) = get_token_abs( statement-from ).

      DATA(if_structure) = COND #( WHEN token = 'IF' THEN <structure>
                                   WHEN token = 'ELSEIF' THEN structures[ <structure>-back ] ).

      IF if_structure IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(condition) = get_token_abs( statement-from + 1 ).

      TRY.
          counters[ if_structure = if_structure
                    condition = condition ]-count = counters[ if_structure = if_structure
                                                              condition = condition ]-count + 1.
        CATCH cx_sy_itab_line_not_found.
          counters = VALUE #( BASE counters
                            ( if_structure = if_structure
                              if_statement = statements[ if_structure-stmnt_from ]
                              condition = condition
                              count = 1 ) ).
      ENDTRY.

    ENDLOOP.

    handle_result( counters ).

  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD should_skip_test_code.
    CHECK test_code_detector->is_testcode( structure ) = abap_true.
    CHECK line_exists( check_configurations[ apply_on_testcode = abap_false ] ).
    result = abap_true.
  ENDMETHOD.


  METHOD handle_result.
    LOOP AT counters ASSIGNING FIELD-SYMBOL(<counter>).

      DATA(configuration) = detect_check_configuration( error_count = <counter>-count
                                                        statement = <counter>-if_statement ).

      IF configuration IS INITIAL.
        CONTINUE.
      ENDIF.

      raise_error( statement_level     = <counter>-if_statement-level
                   statement_index     = <counter>-if_structure-stmnt_from
                   statement_from      = <counter>-if_statement-from
                   error_priority      = configuration-prio ).
    ENDLOOP.
  ENDMETHOD.

  METHOD has_multiple_conditions.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str = 'AND' OR str = 'OR'.
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

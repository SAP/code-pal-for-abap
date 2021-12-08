CLASS y_pal_chain_decl_usage DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS get_chained_statements RETURNING VALUE(result) TYPE sstmnt_tab.
    METHODS get_chained_variables IMPORTING structure_index TYPE sy-tabix
                                  RETURNING VALUE(result) TYPE sstmnt_tab.
    METHODS get_chained_structures IMPORTING structure_index TYPE sy-tabix
                                   RETURNING VALUE(result) TYPE sstmnt_tab.
    METHODS is_chained_structure RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_complex_structure RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_chain_decl_usage IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CHAIN_DECL_USAG' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }chain-declaration-usage.md|.

    relevant_statement_types = VALUE #( BASE relevant_statement_types
                                      ( scan_struc_stmnt_type-public_section )
                                      ( scan_struc_stmnt_type-protected_section )
                                      ( scan_struc_stmnt_type-private_section ) ).

    set_check_message( 'Do not chain up-front declarations!' ).
  ENDMETHOD.


  METHOD inspect_statements.
    DATA(statements) = get_chained_statements( ).

    LOOP AT statements INTO statement_wa
    GROUP BY statement_wa-colonrow.
        IF keyword( ) <> if_kaizen_keywords_c=>gc_data
        AND keyword( ) <> if_kaizen_keywords_c=>gc_class_data
        AND keyword( ) <> if_kaizen_keywords_c=>gc_constants
        AND keyword( ) <> if_kaizen_keywords_c=>gc_types.
          CONTINUE.
        ENDIF.

        DATA(count) = REDUCE i( INIT x = 0
                                FOR row IN statements
                                WHERE ( colonrow = statement_wa-colonrow )
                                NEXT x = x + 1 ).

        IF count = 1.
          CONTINUE.
        ENDIF.

        DATA(configuration) = detect_check_configuration( statement_wa ).

        raise_error( statement_level = statement_wa-level
                     statement_index = statement_wa-number
                     statement_from = statement_wa-from
                     check_configuration = configuration ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD get_chained_statements.
    DATA(index) = line_index( ref_scan->structures[ table_line = structure_wa ] ).
    APPEND LINES OF get_chained_variables( index ) TO result.
    APPEND LINES OF get_chained_structures( index ) TO result.
  ENDMETHOD.


  METHOD get_chained_variables.
    result = VALUE sstmnt_tab( FOR statement IN ref_scan->statements
                               FROM structure_wa-stmnt_from TO structure_wa-stmnt_to
                               WHERE ( struc = structure_index AND prefixlen > 0 )
                               ( statement ) ).
  ENDMETHOD.


  METHOD get_chained_structures.
    DATA(chained_structures) = VALUE sstmnt_tab( FOR statement IN ref_scan->statements
                                                 FROM structure_wa-stmnt_from TO structure_wa-stmnt_to
                                                 WHERE ( struc <> structure_index AND prefixlen > 0 )
                                                 ( statement ) ).

    LOOP AT chained_structures ASSIGNING FIELD-SYMBOL(<chained_structure>)
    GROUP BY <chained_structure>-struc.
      structure_wa = ref_scan->structures[ <chained_structure>-struc ].

      IF is_chained_structure( ) = abap_false
      OR is_complex_structure( ) = abap_true.
        CONTINUE.
      ENDIF.

      APPEND <chained_structure> TO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_chained_structure.
    result = xsdbool( structure_wa-stmnt_type = scan_struc_stmnt_type-data
                   OR structure_wa-stmnt_type = scan_struc_stmnt_type-sequence
                   OR structure_wa-stmnt_type = scan_struc_stmnt_type-types ).
  ENDMETHOD.


  METHOD is_complex_structure.
    DATA(statement_type) = ref_scan->structures[ structure_wa-back ]-stmnt_type.

    result = xsdbool( statement_type = scan_struc_stmnt_type-data
                   OR statement_type = scan_struc_stmnt_type-types ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS y_check_collect DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS extract_table_name IMPORTING statement     TYPE sstmnt
                               RETURNING  VALUE(result) TYPE string.

    METHODS find_internal_table IMPORTING structure     TYPE sstruc
                                          table_name    TYPE string
                                RETURNING VALUE(result) TYPE string.

ENDCLASS.



CLASS y_check_collect IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC COLLECT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }collect.md|.

    set_check_message( 'Only use the statement COLLECT for hashed tables or sorted tables with a unique key!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'COLLECT'.

    DATA(table) = extract_table_name( statement ).

    " COLLECT in header line
    IF table IS INITIAL.
      RETURN.
    ENDIF.

    DATA(declaration) = find_internal_table( structure = structure
                                             table_name = table ).

    " INTERNAL TABLE declaration out of scope
    IF declaration IS INITIAL.
      RETURN.
    ENDIF.

    " INTERNAL TABLE declared as DDIC table type
    IF declaration NS 'TABLE'.
      RETURN.
    ENDIF.

    IF declaration CS 'SORTED TABLE'
    AND declaration CS 'WITH UNIQUE KEY'.
      RETURN.
    ENDIF.

    IF declaration CS 'HASHED TABLE'
    AND declaration CS 'WITH UNIQUE KEY'.
      RETURN.
    ENDIF.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = configuration-prio ).
  ENDMETHOD.


  METHOD extract_table_name.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str = 'INTO'.
        result = get_token_abs( sy-tabix + 1 ).
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD find_internal_table.
    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to.
      IF get_token_abs( <statement>-from ) <> 'DATA'.
        CONTINUE.
      ENDIF.
      DATA(tokens) = condense_tokens( <statement> ).
      IF tokens NS table_name.
        CONTINUE.
      ENDIF.
      result = tokens.
      RETURN.
    ENDLOOP.
    IF result IS INITIAL
    AND structure-back > 0.
      DATA(back) = ref_scan_manager->structures[ structure-back ].
      result = find_internal_table( structure = back
                                    table_name = table_name ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.

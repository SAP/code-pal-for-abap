CLASS y_check_collect DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS extract_table_name
      IMPORTING
        statement     TYPE sstmnt
      RETURNING
        VALUE(result) TYPE string.
  PRIVATE SECTION.
    METHODS find_internal_table
      IMPORTING
        structure     TYPE sstruc
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.
    METHODS extract_table_type
      IMPORTING
        declaration   TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS y_check_collect IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC COLLECT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }collect.md|.

    relevant_statement_types = VALUE #( BASE relevant_statement_types
                                      ( scan_struc_stmnt_type-class_definition ) ).

    set_check_message( 'Only use the statement COLLECT for hashed tables or sorted tables with a unique key!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'COLLECT'.

    DATA(table) = extract_table_name( statement ).
    DATA(declaration) = find_internal_table( structure = structure name = table ).

    IF declaration IS INITIAL.
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
      IF tokens NS name.
        CONTINUE.
      ENDIF.
      result = tokens.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD extract_table_type.

  ENDMETHOD.

ENDCLASS.

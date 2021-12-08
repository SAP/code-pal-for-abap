CLASS y_pal_collect DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS extract_itab_name IMPORTING statement     TYPE sstmnt
                              RETURNING VALUE(result) TYPE string.

    METHODS find_itab_declaration IMPORTING structure     TYPE sstruc
                                            name          TYPE string
                                  RETURNING VALUE(result) TYPE sstmnt.

    METHODS extract_itab_type IMPORTING statement     TYPE sstmnt
                              RETURNING VALUE(result) TYPE string.

    METHODS get_table_rtti IMPORTING table_name    TYPE string
                           RETURNING VALUE(result) TYPE REF TO cl_abap_tabledescr.

ENDCLASS.



CLASS Y_PAL_COLLECT IMPLEMENTATION.


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

    DATA(itab_name) = extract_itab_name( statement ).

    DATA(itab_declaration) = find_itab_declaration( structure = structure
                                                    name = itab_name ).

    " local declaration out of scope
    IF itab_declaration IS INITIAL.
      RETURN.
    ENDIF.

    DATA(declaration_tokens) = condense_tokens( itab_declaration ).

    IF declaration_tokens CS 'SORTED TABLE OF'
    AND declaration_tokens CS 'WITH UNIQUE KEY'.
      RETURN.
    ENDIF.

    IF declaration_tokens CS 'HASHED TABLE OF'
    AND declaration_tokens CS 'WITH UNIQUE KEY'.
      RETURN.
    ENDIF.

    " global table type (DDIC)
    IF declaration_tokens NP '*TYPE *TABLE OF*'
    AND declaration_tokens NP '*LIKE *TABLE OF*'.
      DATA(itab_type) = extract_itab_type( itab_declaration ).
      DATA(type_rtti) = get_table_rtti( itab_type ).

      IF type_rtti IS NOT INITIAL.
        IF type_rtti->table_kind = cl_abap_tabledescr=>tablekind_sorted
        AND type_rtti->has_unique_key = abap_true.
          RETURN.
        ENDIF.

        IF type_rtti->table_kind = cl_abap_tabledescr=>tablekind_hashed
        AND type_rtti->has_unique_key = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD extract_itab_name.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str = 'INTO'.
        result = get_token_abs( sy-tabix + 1 ).
        RETURN.
      ENDIF.
    ENDLOOP.
    " Header Line
    IF statement-to - statement-from = 1.
      result = get_token_abs( statement-to ).
    ENDIF.
  ENDMETHOD.


  METHOD find_itab_declaration.
    LOOP AT ref_scan->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to.
      DATA(first_token) = get_token_abs( <statement>-from ).

      IF first_token <> 'DATA'
      AND first_token <> 'TYPES'
      AND first_token <> 'CLASS-DATA'.
        CONTINUE.
      ENDIF.

      DATA(tokens) = condense_tokens( <statement> ).

      IF  tokens NP |DATA { name } TYPE *|
      AND tokens NP |DATA { name } LIKE *|
      AND tokens NP |TYPES* { name }* TYPE *|
      AND tokens NP |TYPES* { name }* LIKE *|
      AND tokens NP |CLASS-DATA* { name }* TYPE *|
      AND tokens NP |CLASS-DATA* { name }* LIKE *|.
        CONTINUE.
      ENDIF.

      IF tokens CP '* TABLE OF *'.
        result = <statement>.
        RETURN.
      ENDIF.

      DATA(local_typed) = find_itab_declaration( structure = structure
                                                 name = extract_itab_type( <statement> ) ).

      result = COND #( WHEN local_typed IS NOT INITIAL THEN local_typed
                       ELSE <statement> ).

      RETURN.
    ENDLOOP.

    IF structure-back > 0.
      result = find_itab_declaration( structure = ref_scan->structures[ structure-back ]
                                      name = name ).
    ENDIF.
  ENDMETHOD.


  METHOD extract_itab_type.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str = 'OF'.
      IF get_token_abs( sy-tabix - 1 ) = 'TABLE'.
        result = get_token_abs( sy-tabix + 1 ).
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT ref_scan->tokens ASSIGNING <token>
    FROM statement-from TO statement-to
    WHERE str = 'TYPE'
    OR str = 'LIKE'.
      result = get_token_abs( sy-tabix + 1 ).
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_table_rtti.
    cl_abap_elemdescr=>describe_by_name(
      EXPORTING
        p_name         = table_name
      RECEIVING
        p_descr_ref = DATA(rtti_type)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        result ?= rtti_type.
      CATCH cx_sy_move_cast_error.
        " Not a TableDescr type
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

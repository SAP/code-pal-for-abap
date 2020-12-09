CLASS y_check_declaration_in_if DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_scope_dependent IMPORTING structure     TYPE sstruc
                               RETURNING value(result) TYPE abap_bool.

    METHODS extract_variable_name IMPORTING token         TYPE string
                                  RETURNING value(result) TYPE string.

ENDCLASS.



CLASS Y_CHECK_DECLARATION_IN_IF IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC DECL_IN_IF' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-prio = c_warning.
    settings-documentation = |{ c_docs_path-checks }declaration-in-if.md|.

    set_check_message( 'Declarations in IF-Blocks should be removed!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(token) = get_token_abs( statement-from ).

    IF token NP 'DATA(*)'
    OR token NP 'FIELD-SYMBOLS(*)'.
      RETURN.
    ENDIF.

    DATA(variable_name) = extract_variable_name( token ).

    DATA(structures) = ref_scan_manager->get_structures( ).
    DATA(strucutre_of_statement) = structures[ statement-struc ].

    IF is_scope_dependent( strucutre_of_statement ) = abap_false.
      RETURN.
    ENDIF.

    LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to
    WHERE number > statement-number.
      IF get_token_abs( <statement>-from ) <> variable_name.
        CONTINUE.
      ENDIF.

      DATA(check_configuration) = detect_check_configuration( statement ).

      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio ).
    ENDLOOP.
  ENDMETHOD.


  METHOD is_scope_dependent.
    result = xsdbool(    structure-stmnt_type = scan_struc_stmnt_type-if
                      OR structure-stmnt_type = scan_struc_stmnt_type-elseif
                      OR structure-stmnt_type = scan_struc_stmnt_type-else
                      OR structure-stmnt_type = scan_struc_stmnt_type-do
                      OR structure-stmnt_type = scan_struc_stmnt_type-case
                      OR structure-stmnt_type = scan_struc_stmnt_type-loop
                      OR structure-stmnt_type = scan_struc_stmnt_type-while ).
  ENDMETHOD.


  METHOD extract_variable_name.
    result = token.
    REPLACE ALL OCCURRENCES OF 'DATA(' IN result WITH ''.
    REPLACE ALL OCCURRENCES OF 'FIELD-SYMBOLS(' IN result WITH ''.
    REPLACE ALL OCCURRENCES OF ')' IN result WITH ''.
  ENDMETHOD.

ENDCLASS.

CLASS y_pal_scope_of_variable DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS is_isolated IMPORTING strucutre_row TYPE stmnt_stru
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS extract_variable_name IMPORTING token         TYPE string
                                  RETURNING VALUE(result) TYPE string.

    METHODS get_scope_structure IMPORTING strucutre_row TYPE stmnt_stru
                                RETURNING VALUE(result) TYPE sstruc.

ENDCLASS.



CLASS y_pal_scope_of_variable IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC SCOPE_OF_VAR' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }scope-of-variable.md|.

    set_check_message( 'Variable in use out of its scope!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    DATA(token) = get_token_abs( statement-from ).

    IF token NP 'DATA(*)'
    AND token NP 'FIELD-SYMBOLS(*)'.
      RETURN.
    ENDIF.

    IF is_isolated( statement-struc ) = abap_false.
      RETURN.
    ENDIF.

    DATA(variable) = extract_variable_name( token ).
    DATA(scope) = get_scope_structure( statement-struc ).
    DATA(statement_index) = index.

    LOOP AT ref_scan->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM scope-stmnt_from TO scope-stmnt_to
    WHERE number > statement-number.

      statement_index = statement_index + 1.

      IF <statement>-struc = statement-struc.
        CONTINUE.
      ENDIF.

      LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS
      FROM <statement>-from TO <statement>-to
      WHERE str = variable.
        DATA(check_configuration) = detect_check_configuration( <statement> ).

        raise_error( statement_level = <statement>-level
                     statement_index = statement_index
                     statement_from = <statement>-from
                     check_configuration = check_configuration ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD is_isolated.
    DATA(structure) = ref_scan->structures[ strucutre_row ].

    result = xsdbool(    structure-stmnt_type = scan_struc_stmnt_type-if
                      OR structure-stmnt_type = scan_struc_stmnt_type-then
                      OR structure-stmnt_type = scan_struc_stmnt_type-elseif
                      OR structure-stmnt_type = scan_struc_stmnt_type-else
                      OR structure-stmnt_type = scan_struc_stmnt_type-do
                      OR structure-stmnt_type = scan_struc_stmnt_type-case
                      OR structure-stmnt_type = scan_struc_stmnt_type-when
                      OR structure-stmnt_type = scan_struc_stmnt_type-loop
                      OR structure-stmnt_type = scan_struc_stmnt_type-while ).
  ENDMETHOD.


  METHOD extract_variable_name.
    result = token.
    REPLACE ALL OCCURRENCES OF 'DATA(' IN result WITH ''.
    REPLACE ALL OCCURRENCES OF 'FIELD-SYMBOLS(' IN result WITH ''.
    REPLACE ALL OCCURRENCES OF ')' IN result WITH ''.
  ENDMETHOD.


  METHOD get_scope_structure.
    DATA(structure) = ref_scan->structures[ strucutre_row ].

    DATA(is_root) = xsdbool(    structure-stmnt_type = scan_struc_stmnt_type-form
                             OR structure-stmnt_type = scan_struc_stmnt_type-method
                             OR structure-stmnt_type = scan_struc_stmnt_type-function
                             OR structure-stmnt_type = scan_struc_stmnt_type-module
                             OR structure-type       = scan_struc_type-event ).

    result = COND #( WHEN is_root = abap_true THEN structure
                                              ELSE get_scope_structure( structure-back ) ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

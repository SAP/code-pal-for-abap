CLASS y_check_db_access_in_ut DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_structures REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF risk_level,
                 harmless  TYPE string VALUE 'HARMLESS',
                 dangerous TYPE string VALUE 'DANGEROUS',
                 critical  TYPE string VALUE 'CRITICAL',
               END OF risk_level.

    CONSTANTS: BEGIN OF check_for,
                 alter    TYPE char40 VALUE 'ALTER',
                 delete   TYPE char40 VALUE 'DELETE',
                 update   TYPE char40 VALUE 'UPDATE',
                 modify   TYPE char40 VALUE 'MODIFY',
                 insert   TYPE char40 VALUE 'INSERT',
                 select   TYPE char40 VALUE 'SELECT',
                 commit   TYPE char40 VALUE 'COMMIT',
                 rollback TYPE char40 VALUE 'ROLLBACK',
               END OF check_for.

    CONSTANTS: BEGIN OF framework,
                 qsql_if TYPE char40 VALUE 'IF_OSQL_TEST_ENVIRONMENT',
                 qsql_cl TYPE char40 VALUE 'CL_OSQL_TEST_ENVIRONMENT',
                 cds_if  TYPE char40 VALUE 'IF_CDS_TEST_ENVIRONMENT',
                 cds_cl  TYPE char40 VALUE 'CL_CDS_TEST_ENVIRONMENT',
               END OF framework.

    CONSTANTS: BEGIN OF keys,
                 from  TYPE char40 VALUE 'FROM',
                 into  TYPE char40 VALUE 'INTO',
                 class TYPE char40 VALUE 'CLASS',
                 table TYPE char4 VALUE 'TABL',
               END OF keys.

    TYPES: BEGIN OF properties,
             name       TYPE string,
             risk_level TYPE string,
           END OF properties.

    DATA defined_classes TYPE STANDARD TABLE OF properties.

    METHODS get_class_name IMPORTING structure     TYPE sstruc
                           RETURNING VALUE(result) TYPE string
                           RAISING   cx_sy_itab_line_not_found.

    METHODS get_risk_level IMPORTING statement     TYPE sstmnt
                           RETURNING VALUE(result) TYPE string.

    METHODS add_line_to_defined_classes IMPORTING statement TYPE sstmnt
                                                  structure TYPE sstruc.

    METHODS check_class IMPORTING index     TYPE i
                                  statement TYPE sstmnt
                                  structure TYPE sstruc.

    METHODS is_part_of_framework IMPORTING structure     TYPE sstruc
                                 RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_persistent_object IMPORTING obj_name      TYPE tadir-obj_name
                                 RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_internal_table IMPORTING statement     TYPE sstmnt
                              RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_forbidden_tokens IMPORTING class_name    TYPE string
                                 RETURNING VALUE(result) TYPE y_char255_tab.

    METHODS has_ddic_itab_same_syntax IMPORTING token         TYPE stokesx
                                      RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_CHECK_DB_ACCESS_IN_UT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC DB_ACCESS_UT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_productive_code = abap_false.
    settings-apply_on_test_code = abap_true.
    settings-documentation = |{ c_docs_path-checks }db-access-in-ut.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-class_implementation ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Database access(es) within a Unit-Test should be removed!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CASE structure-stmnt_type.
      WHEN scan_struc_stmnt_type-class_definition.
        add_line_to_defined_classes( statement = statement
                                     structure = structure ).

      WHEN scan_struc_stmnt_type-class_implementation.
        check_class( index = index
                     statement = statement
                     structure = structure ).

    ENDCASE.
  ENDMETHOD.


  METHOD is_persistent_object.
    DATA(upper_name) = to_upper( obj_name ).

    SELECT SINGLE @abap_true
    FROM tadir
    INTO @result
    WHERE obj_name = @upper_name
    AND object = @keys-table
    AND delflag = @space.
  ENDMETHOD.


  METHOD is_internal_table.
    DATA(second_token) = get_token_abs( statement-from + 1 ).
    DATA(third_token) = get_token_abs( statement-from + 2 ).
    DATA(fourth_token) = get_token_abs( statement-from + 2 ).

    IF second_token = keys-into.
      RETURN.
    ENDIF.

    DATA(table_name) = COND #( WHEN second_token = keys-from THEN third_token
                               WHEN third_token = keys-into  THEN fourth_token
                               ELSE second_token ).

    IF strlen( table_name ) > 40.
      RETURN.
    ENDIF.

    result = xsdbool( is_persistent_object( CONV #( table_name ) ) = abap_false ).
  ENDMETHOD.


  METHOD add_line_to_defined_classes.
    TRY.
        DATA(class_config) = VALUE properties( name = get_class_name( structure ) ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF line_exists( defined_classes[ name = class_config-name ] ).
      RETURN.
    ENDIF.

    class_config-risk_level = get_risk_level( statement ).

    IF is_part_of_framework( structure ) = abap_false.
      APPEND class_config TO defined_classes.
    ENDIF.
  ENDMETHOD.


  METHOD check_class.
    TRY.
        DATA(class_name) = get_class_name( structure ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF NOT line_exists( defined_classes[ name = class_name ] ).
      RETURN.
    ENDIF.

    DATA(forbidden_tokens) = get_forbidden_tokens( class_name ).

    DATA(token) = ref_scan_manager->tokens[ statement-from ].

    IF NOT line_exists( forbidden_tokens[ table_line = token-str ] ).
      RETURN.
    ENDIF.

    IF has_ddic_itab_same_syntax( token ) = abap_true
     AND is_internal_table( statement ) = abap_true.
      RETURN.
    ENDIF.

    IF ref_scan_manager->tokens[ statement-from + 1 ]-str = '='.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = check_configuration-prio ).
  ENDMETHOD.


  METHOD get_forbidden_tokens.
    DATA risk_lvl TYPE properties-risk_level.
    TRY.
        risk_lvl = defined_classes[ name = class_name ]-risk_level.
      CATCH cx_sy_itab_line_not_found.
        risk_lvl = space.
    ENDTRY.

    CASE risk_lvl.
      WHEN risk_level-dangerous OR risk_level-critical.
        result = VALUE #( ( check_for-alter )
                          ( check_for-delete )
                          ( check_for-update )
                          ( check_for-modify ) ).
      WHEN OTHERS.
        result = VALUE #( ( check_for-alter )
                          ( check_for-delete )
                          ( check_for-update )
                          ( check_for-modify )
                          ( check_for-insert )
                          ( check_for-select )
                          ( check_for-commit )
                          ( check_for-rollback ) ).
    ENDCASE.
  ENDMETHOD.


  METHOD is_part_of_framework.
    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to.
      LOOP AT ref_scan_manager->tokens TRANSPORTING NO FIELDS
      FROM <statement>-from TO <statement>-to
      WHERE str = framework-qsql_if
      OR str = framework-qsql_cl
      OR str = framework-cds_if
      OR str = framework-cds_cl.
        result = abap_true.
        RETURN.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_class_name.
    DATA(index) = ref_scan_manager->statements[ structure-stmnt_from ]-from.
    IF get_token_abs( index ) = keys-class.
      result = get_token_abs( index + 1 ).
    ENDIF.
  ENDMETHOD.


  METHOD get_risk_level.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str = risk_level-harmless
    OR str = risk_level-dangerous
    OR str = risk_level-critical.
      result = <token>-str.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_structures.
    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition ) ).
    super->inspect_structures( ).

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_implementation ) ).
    super->inspect_structures( ).
  ENDMETHOD.


  METHOD has_ddic_itab_same_syntax.
    result = xsdbool(  token-str = check_for-modify
                    OR token-str = check_for-update
                    OR token-str = check_for-delete
                    OR token-str = check_for-insert ).
  ENDMETHOD.


ENDCLASS.

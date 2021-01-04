CLASS y_check_db_access_in_ut DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS risk_level_harmless TYPE string VALUE 'HARMLESS'.
    CONSTANTS risk_level_dangerous TYPE string VALUE 'DANGEROUS'.
    CONSTANTS risk_level_critical TYPE string VALUE 'CRITICAL'.
    CONSTANTS risk_level_not_set TYPE string VALUE 'NOT_SET'.

    DATA tokens_not_allowed TYPE y_char255_tab.

    METHODS has_osql_or_cds_framework IMPORTING method        TYPE sstruc
                                      RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_persistent_object IMPORTING obj_name      TYPE string
                                 RETURNING VALUE(result) TYPE abap_bool.

    METHODS consolidade_tokens IMPORTING statement     TYPE sstmnt
                               RETURNING VALUE(result) TYPE string.

    METHODS get_class_definition IMPORTING method        TYPE sstruc
                                 RETURNING VALUE(result) TYPE sstruc
                                 RAISING   cx_sy_itab_line_not_found.

    METHODS get_test_risk_level IMPORTING method        TYPE sstruc
                                RETURNING VALUE(result) TYPE string.

    METHODS determine_tokens_not_allowed IMPORTING method TYPE sstruc.

    METHODS has_ddic_itab_same_syntax IMPORTING token         TYPE char255
                                      RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_internal_table IMPORTING statement     TYPE sstmnt
                              RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_check_db_access_in_ut IMPLEMENTATION.


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

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-method ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Database access(es) within a Unit-Test should be removed!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK has_osql_or_cds_framework( structure ) = abap_false.

    determine_tokens_not_allowed( structure ).

    DATA(tokens) = consolidade_tokens( statement ).

    LOOP AT tokens_not_allowed ASSIGNING FIELD-SYMBOL(<token_not_allowed>).
      IF tokens NP <token_not_allowed>.
        CONTINUE.
      ENDIF.

      IF has_ddic_itab_same_syntax( <token_not_allowed> ) = abap_true
      AND is_internal_table( statement ) = abap_true.
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


  METHOD is_persistent_object.
    cl_abap_structdescr=>describe_by_name( EXPORTING p_name = obj_name
                                           EXCEPTIONS OTHERS = 1 ).
    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD has_osql_or_cds_framework.
    TRY.
        DATA(class_definition) = get_class_definition( method ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM class_definition-stmnt_from TO class_definition-stmnt_to.
      LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
      FROM <statement>-from TO <statement>-to
      WHERE str = 'IF_OSQL_TEST_ENVIRONMENT'
      OR str = 'CL_OSQL_TEST_ENVIRONMENT'
      OR str = 'IF_CDS_TEST_ENVIRONMENT'
      OR str = 'CL_CDS_TEST_ENVIRONMENT'.
        result = abap_true.
        RETURN.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD consolidade_tokens.
    LOOP AT ref_scan_manager->tokens INTO DATA(token)
    FROM statement-from TO statement-to.
      token-str = condense( token-str ).
      result = COND #( WHEN result IS INITIAL THEN token-str
                                              ELSE |{ result } { token-str }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_class_definition.
    DATA(class_implementation) = ref_scan_manager->structures[ method-back ].
    result = ref_scan_manager->structures[ class_implementation-back ].
  ENDMETHOD.


  METHOD get_test_risk_level.
    TRY.
        DATA(class_definition) = get_class_definition( method ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM class_definition-stmnt_from TO class_definition-stmnt_to.
      DATA(tokens) = consolidade_tokens( <statement> ).
      result = COND #( WHEN tokens CS 'RISK LEVEL HARMLESS' THEN risk_level_harmless
                       WHEN tokens CS 'RISK LEVEL DANGEROUS' THEN risk_level_dangerous
                       WHEN tokens CS 'RISK LEVEL CRITICAL' THEN risk_level_critical ).
      IF result IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.

    result = risk_level_not_set.
  ENDMETHOD.


  METHOD determine_tokens_not_allowed.
    DATA(test_risk_level) = get_test_risk_level( method ).

    tokens_not_allowed = COND #( WHEN test_risk_level = risk_level_harmless  THEN VALUE #( ( 'ALTER *' ) ( 'DELETE *' ) ( 'UPDATE *' ) ( 'MODIFY *' ) ( 'INSERT INTO *' ) ( 'SELECT *' )  ( 'COMMIT*' ) ( 'ROLLBACK*' ) )
                                 WHEN test_risk_level = risk_level_not_set   THEN VALUE #( ( 'ALTER *' ) ( 'DELETE *' ) ( 'UPDATE *' ) ( 'MODIFY *' ) ( 'INSERT INTO *' ) ( 'SELECT *' )  ( 'COMMIT*' ) ( 'ROLLBACK*' ) )
                                 WHEN test_risk_level = risk_level_dangerous THEN VALUE #( ( 'ALTER *' ) ( 'DELETE *' ) ( 'UPDATE *' ) ( 'MODIFY *' ) )
                                 WHEN test_risk_level = risk_level_critical  THEN VALUE #( ( 'ALTER *' ) ( 'DELETE *' ) ( 'UPDATE *' ) ( 'MODIFY *' ) ) ).
  ENDMETHOD.


  METHOD has_ddic_itab_same_syntax.
    result = xsdbool(    token CS 'MODIFY'
                      OR token CS 'UPDATE'
                      OR token CS 'DELETE' ).
  ENDMETHOD.


  METHOD is_internal_table.
    DATA(second_token) = get_token_abs( statement-from + 1 ).
    DATA(tirth_token) = get_token_abs( statement-from + 2 ).

    DATA(table) = COND #( WHEN second_token = 'FROM' THEN tirth_token
                                                     ELSE second_token ).

    result = xsdbool( is_persistent_object( table ) = abap_false ).
  ENDMETHOD.

ENDCLASS.

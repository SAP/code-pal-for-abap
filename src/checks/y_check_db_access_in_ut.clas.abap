CLASS y_check_db_access_in_ut DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF tokens_not_allowed_type,
             for_harmless  TYPE y_char255_tab,
             for_dangerous TYPE y_char255_tab,
             for_critical  TYPE y_char255_tab,
             for_not_set   TYPE y_char255_tab,
           END OF tokens_not_allowed_type.

    CONSTANTS risk_level_harmless TYPE string VALUE 'HARMLESS'.
    CONSTANTS risk_level_dangerous TYPE string VALUE 'DANGEROUS'.
    CONSTANTS risk_level_critical TYPE string VALUE 'CRITICAL'.
    CONSTANTS risk_level_not_set TYPE string VALUE 'NOT_SET'.

    DATA tokens_not_allowed TYPE tokens_not_allowed_type.
    DATA test_risk_level TYPE string.

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

    set_check_message( 'Database access(es) within a Unit-Test should be removed!' ).

    tokens_not_allowed = VALUE #( for_harmless = VALUE #( ( 'ALTER *' ) ( 'DELETE *' ) ( 'UPDATE *' ) ( 'MODIFY *' ) ( 'INSERT INTO *' ) ( 'SELECT *' )  ( 'COMMIT*' ) ( 'ROLLBACK*' ) )
                                  for_critical = VALUE #( ( 'ALTER *' ) ( 'DELETE *' ) ( 'UPDATE *' ) ( 'MODIFY *' ) ) ).

    tokens_not_allowed-for_not_set   = tokens_not_allowed-for_harmless.
    tokens_not_allowed-for_dangerous = tokens_not_allowed-for_critical.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
    WHERE stmnt_type EQ scan_struc_stmnt_type-method.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      IF is_testcode = abap_false.
        CONTINUE.
      ENDIF.

      IF has_osql_or_cds_framework( <structure> ) = abap_true.
        CONTINUE.
      ENDIF.

      test_risk_level = get_test_risk_level( <structure> ).

      DATA(index) = <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
      FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( index = index
                        statement = <statement> ).

        index = index + 1.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.

    DATA(source_code) = consolidade_tokens( statement ).

    DATA(tokens) = COND #( WHEN test_risk_level = risk_level_harmless THEN tokens_not_allowed-for_harmless
                           WHEN test_risk_level = risk_level_dangerous THEN tokens_not_allowed-for_dangerous
                           WHEN test_risk_level = risk_level_critical THEN tokens_not_allowed-for_critical
                           WHEN test_risk_level = risk_level_not_set THEN tokens_not_allowed-for_not_set ).

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<not_allowed_token>).
      IF source_code NP <not_allowed_token>.
        CONTINUE.
      ENDIF.

      " Same syntax (DDIC/ITAB)
      IF <not_allowed_token> CS 'MODIFY'
      OR <not_allowed_token> CS 'UPDATE'
      OR <not_allowed_token> CS 'DELETE'.
        " Might have 'FROM'
        IF is_persistent_object( get_token_abs( statement-from + 1 ) ) = abap_false
        AND is_persistent_object( get_token_abs( statement-from + 2 ) ) = abap_false.
          CONTINUE.
        ENDIF.
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

    LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
    FROM class_definition-stmnt_from TO class_definition-stmnt_to.
      LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
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
    LOOP AT ref_scan_manager->get_tokens( ) INTO DATA(token)
    FROM statement-from TO statement-to.
      token-str = condense( token-str ).
      result = COND #( WHEN result IS INITIAL THEN token-str
                                              ELSE |{ result } { token-str }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_class_definition.
    DATA(structures) = ref_scan_manager->get_structures( ).
    DATA(class_implementation) = structures[ method-back ].
    result = structures[ class_implementation-back ].
  ENDMETHOD.


  METHOD get_test_risk_level.
    TRY.
        DATA(class_definition) = get_class_definition( method ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
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


ENDCLASS.

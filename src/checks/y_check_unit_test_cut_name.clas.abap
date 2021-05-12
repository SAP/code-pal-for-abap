CLASS y_check_unit_test_cut_name DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_for_testing IMPORTING statement TYPE sstmnt
                           RETURNING VALUE(result) TYPE abap_bool.

    METHODS has_cut IMPORTING structure TYPE sstruc
                    RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_check_unit_test_cut_name IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC UT_CUT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-apply_on_productive_code = abap_false.
    settings-apply_on_test_code = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }unit_test_cut_name.md|.
    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-method ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Consider naming the class under test to `CUT`' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHOD'.

    IF is_for_testing( statement ) = abap_false
    OR has_cut( structure ) = abap_true.
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


  METHOD is_for_testing.
    TRY.
        DATA(method_name) = ref_scan_manager->tokens[ statement-from + 1 ]-str.
        DATA(method) = ref_scan_manager->structures[ statement-struc ].
        DATA(class_implementation) = ref_scan_manager->structures[ method-back ].
        DATA(class_definition) = ref_scan_manager->structures[ class_implementation-back ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM class_definition-stmnt_from TO class_definition-stmnt_to.

      IF get_token_abs( <statement>-from ) <> 'METHODS'
      AND get_token_abs( <statement>-from ) <> 'CLASS-METHODS'.
        CONTINUE.
      ENDIF.

      DATA(statement_abs) = get_statement_abs( <statement> ).

      IF statement_abs NP |*{ method_name }*|.
        CONTINUE.
      ENDIF.

      result = xsdbool( statement_abs CP '*FOR TESTING*' ).
      RETURN.

    ENDLOOP.
  ENDMETHOD.


  METHOD has_cut.
    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to
    WHERE type <> scan_stmnt_type-comment
    AND type <> scan_stmnt_type-pragma.
      DATA(statement_abs) = get_statement_abs( <statement> ).

      result = xsdbool(    statement_abs CP '* CUT *'
                        OR statement_abs CP '*_CUT *'
                        OR statement_abs CP '* CLASS_UNDER_TEST *'
                        OR statement_abs CP '*_CLASS_UNDER_TEST *'  ).

     IF result = abap_true.
       RETURN.
     ENDIF.
   ENDLOOP.
  ENDMETHOD.


ENDCLASS.

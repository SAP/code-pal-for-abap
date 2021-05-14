CLASS y_check_cut_as_default DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_for_testing IMPORTING method_name TYPE string
                                     class_definition TYPE sstruc
                           RETURNING VALUE(result) TYPE abap_bool.

    METHODS has_cut IMPORTING structure TYPE sstruc
                    RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_class_definition IMPORTING structure TYPE sstruc
                                 RETURNING VALUE(result) TYPE sstruc
                                 RAISING cx_sy_itab_line_not_found.

ENDCLASS.


CLASS y_check_cut_as_default IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CUT_AS_DEFAULT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-apply_on_productive_code = abap_false.
    settings-apply_on_test_code = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }cut_as_default.md|.
    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-method ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Give the variable that represents the code under test the `cut` name' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHOD'.

    DATA(class_definition) = get_class_definition( structure ).

    DATA(for_testing) = is_for_testing( method_name      = get_token_abs( statement-from + 1 )
                                        class_definition = class_definition ).

    IF for_testing = abap_false.
      RETURN.
    ENDIF.

    IF has_cut( structure ) = abap_true
    OR has_cut( class_definition ) = abap_true.
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
                        OR statement_abs CP '*_CUT *' ).

     IF result = abap_true.
       RETURN.
     ENDIF.
   ENDLOOP.
  ENDMETHOD.


  METHOD get_class_definition.
    DATA(class_implementation) = ref_scan_manager->structures[ structure-back ].
    result = ref_scan_manager->structures[ class_implementation-back ].
  ENDMETHOD.


ENDCLASS.

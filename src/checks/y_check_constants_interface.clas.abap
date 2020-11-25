CLASS y_check_constants_interface DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA has_something_else TYPE abap_bool VALUE abap_false.
    DATA statement_for_message TYPE sstmnt.
    METHODS checkif_error IMPORTING index TYPE i.
    METHODS is_structure_empty IMPORTING structure TYPE sstruc RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_CHECK_CONSTANTS_INTERFACE IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( statement_for_message ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF has_something_else EQ abap_false.
      raise_error( statement_level     = statement_for_message-level
                   statement_index     = index
                   statement_from      = statement_for_message-from
                   error_priority      = check_configuration-prio ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CONS_INTF' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }constants-interface.md|.

    set_check_message( 'There are only constants in this interface!' ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
          WHERE stmnt_type EQ scan_struc_stmnt_type-interface.

      IF is_structure_empty( <structure> ) = abap_true.
        CONTINUE.
      ENDIF.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      READ TABLE ref_scan_manager->get_statements( ) INTO statement_for_message
        INDEX <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to WHERE type NE scan_stmnt_type-comment AND
                                                                  type NE scan_stmnt_type-comment_in_stmnt AND
                                                                  type NE scan_stmnt_type-pragma.
        inspect_tokens( statement = <statement> ).
      ENDLOOP.

      checkif_error( <structure>-stmnt_from ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(token) = get_token_abs( statement-from ).
    IF token NE 'CONSTANTS' AND
       token NE 'INTERFACE' AND
       token NE 'ENDINTERFACE' AND
       token NE 'BEGIN' AND
       token NE 'END' AND
       token NE 'OF'.
      has_something_else = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_structure_empty.
    result = xsdbool( structure-stmnt_from = structure-stmnt_to - 1 ).
  ENDMETHOD.
ENDCLASS.

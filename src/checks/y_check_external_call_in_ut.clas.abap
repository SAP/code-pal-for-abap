CLASS y_check_external_call_in_ut DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS check_if_error IMPORTING index     TYPE i
                                     statement TYPE sstmnt .

ENDCLASS.



CLASS y_check_external_call_in_ut IMPLEMENTATION.


  METHOD check_if_error.
    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = check_configuration-prio
                 parameter_01    = get_token_abs( statement-from ) ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC EXT_CALL_UT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_productive_code = abap_false.
    settings-apply_on_test_code = abap_true.
    settings-documentation = |{ c_docs_path-checks }external-call-in-ut.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-method ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'External Call from an Unit-Test should be removed!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA has_redirection TYPE abap_bool VALUE abap_false.

    DATA(token1) = get_token_abs( statement-from ).
    DATA(token2) = get_token_abs( statement-from + 1 ).
    DATA(token3) = get_token_abs( statement-from + 2 ).
    DATA(token4) = get_token_abs( statement-from + 3 ).
    DATA(token5) = get_token_abs( statement-from + 4 ).
    DATA(token6) = get_token_abs( statement-from + 5 ).
    DATA(token4to6) = |{ token4 } { token5 } { token6 }|.

    CASE token1.
      WHEN 'SUBMIT'.
        has_redirection = abap_true.
      WHEN 'CALL'.
        IF token2 = 'FUNCTION'
        AND ( token4 = 'DESTINATION'
              OR token4to6 = 'STARTING NEW TASK'
              OR token4to6 = 'IN UPDATE TASK' ).
          has_redirection = abap_true.
        ELSEIF token2 = 'METHOD'
        AND token3 CS 'CL_GUI_'.
          has_redirection = abap_true.
        ENDIF.
      WHEN OTHERS.
        LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
        FROM statement-from TO statement-to
        WHERE type = 'I'.
          IF <token>-str CS 'CL_GUI_'
          AND <token>-str NS '=>'.
            has_redirection = abap_true.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    IF has_redirection = abap_true.
      check_if_error( index = index
                      statement = statement ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

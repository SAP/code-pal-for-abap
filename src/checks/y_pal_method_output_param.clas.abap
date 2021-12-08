CLASS y_pal_method_output_param DEFINITION PUBLIC  INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA has_exporting_parameter TYPE abap_bool.
    DATA has_changing_parameter TYPE abap_bool.
    DATA has_returning_parameter TYPE abap_bool.

    METHODS has_error RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_PAL_METHOD_OUTPUT_PARAM IMPLEMENTATION.

  METHOD has_error.
    DATA(sum) = 0.
    IF has_exporting_parameter = abap_true.
      sum = sum + 1.
    ENDIF.
    IF has_changing_parameter = abap_true.
      sum = sum + 1.
    ENDIF.
    IF has_returning_parameter = abap_true.
      sum = sum + 1.
    ENDIF.
    IF sum > 1.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PARAMETER_OUT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 1.
    settings-documentation = |{ c_docs_path-checks }method-output-parameter.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-interface ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( 'Combination of parameters(RETURNING/EXPORTING) should not be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHODS'
       OR get_token_abs( statement-from ) = 'CLASS-METHODS'.

    has_exporting_parameter = abap_false.
    has_changing_parameter = abap_false.
    has_returning_parameter = abap_false.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      CASE <token>-str.
        WHEN 'EXPORTING'.
          has_exporting_parameter = abap_true.
        WHEN 'RETURNING'.
          has_returning_parameter = abap_true.
        WHEN 'CHANGING'.
          has_changing_parameter = abap_true.
      ENDCASE.

    ENDLOOP.

    IF has_error( ) = abap_false.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

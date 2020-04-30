CLASS lcl_ref_scan_manager IMPLEMENTATION.
  METHOD y_if_scan_manager~get_structures.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->structures.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_statements.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->statements.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_tokens.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->tokens.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_levels.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->levels.
  ENDMETHOD.

  METHOD y_if_scan_manager~set_ref_scan.
    ref_scan = io_ref_scan.
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    CHECK ref_scan IS NOT INITIAL.

    result = abap_true.
    IF ref_scan->subrc <> ok.
      result = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_pseudo_comment_detector IMPLEMENTATION.
  METHOD lif_pseudo_comment_detector~is_pseudo_comment.
    determine_pseudo_comments(
      EXPORTING scimessages = scimessages
                test        = test
                code        = code
                suppress    = suppress ).

    IF NOT ( pcom = cl_ci_test_root=>c_exceptn_imposibl OR
             pcom = '' ).

      IF position IS NOT INITIAL.
        result = has_comment( ref_scan_manager = ref_scan_manager position = position ).

        IF result <> cl_ci_test_root=>c_pc_exceptn_exists.
          result = has_inline_comment( ref_scan_manager = ref_scan_manager position = position ).
        ENDIF.
      ELSE.
        result = ''.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD has_comment.
    result = cl_ci_test_root=>c_pc_exceptn_posibl.

    DATA(l_position) = position + 1.

    DO.
      READ TABLE ref_scan_manager->get_statements( ) INTO DATA(l_statement_wa) INDEX l_position.
      IF sy-subrc <> 0 OR l_statement_wa-type <> 'P'.
        EXIT.
      ENDIF.

      LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<l_token_wa>)
        FROM l_statement_wa-from TO l_statement_wa-to.

        IF <l_token_wa>-str CS lif_pseudo_comment_detector=>ec_comment AND (
              <l_token_wa>-str CS |{ lif_pseudo_comment_detector=>ec_prefix } { pcom }| OR
              <l_token_wa>-str CS |{ lif_pseudo_comment_detector=>ec_prefix } { pcom_alt }| OR
              <l_token_wa>-str CS |{ lif_pseudo_comment_detector=>ec_prefix } *| ).
          result = cl_ci_test_root=>c_pc_exceptn_exists.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF result = cl_ci_test_root=>c_pc_exceptn_exists.
        EXIT.
      ENDIF.

      l_position = l_position + 1.
    ENDDO.
  ENDMETHOD.

  METHOD has_inline_comment.
    DATA(l_position) = position - 1.

    DO.
      READ TABLE ref_scan_manager->get_statements( ) INTO DATA(l_statement_wa) INDEX l_position.
      IF sy-subrc <> 0 OR ( l_statement_wa-type <> 'S' AND l_statement_wa-type <> 'G' ).
        EXIT.
      ENDIF.

      IF l_statement_wa-type = 'S'.
        LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<l_token_wa>)
          FROM l_statement_wa-from TO l_statement_wa-to.

          IF <l_token_wa>-str CS lif_pseudo_comment_detector=>ec_comment AND (
                <l_token_wa>-str CS |{ lif_pseudo_comment_detector=>ec_prefix } { pcom }| OR
                <l_token_wa>-str CS |{ lif_pseudo_comment_detector=>ec_prefix } { pcom_alt }| OR
                <l_token_wa>-str CS |{ lif_pseudo_comment_detector=>ec_prefix } *| ).
            result = cl_ci_test_root=>c_pc_exceptn_exists.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF result = cl_ci_test_root=>c_pc_exceptn_exists.
          EXIT.
        ENDIF.
      ENDIF.

      l_position = l_position - 1.
    ENDDO.
  ENDMETHOD.

  METHOD determine_pseudo_comments.
    pcom_alt = '*'.

    READ TABLE scimessages INTO DATA(smsg)
         WITH TABLE KEY test = test
                        code = code.
    IF sy-subrc = 0.
      pcom = smsg-pcom.
      IF smsg-pcom_alt IS NOT INITIAL.
        pcom_alt = smsg-pcom_alt.
      ENDIF.
    ELSE.
      IF suppress IS INITIAL.
        pcom = suppress.
      ELSE.
        IF suppress CS lif_pseudo_comment_detector=>ec_comment.
          pcom = suppress+5.
        ELSEIF suppress CS lif_pseudo_comment_detector=>ec_prefix.
          pcom = suppress+4.
        ELSE.
          pcom = suppress.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_statistics IMPLEMENTATION.
  METHOD y_if_scan_statistics~collect.
    IF pc = cl_ci_test_root=>c_pc_exceptn_exists.
      number_pseudo_comments = number_pseudo_comments + 1.
    ELSE.
      IF kind = y_check_base=>c_error.
        number_errors = number_errors + 1.
      ELSEIF kind = y_check_base=>c_warning.
        number_warnings = number_warnings + 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD y_if_scan_statistics~get_number_errors.
    result = number_errors.
  ENDMETHOD.

  METHOD y_if_scan_statistics~get_number_pseudo_comments.
    result = number_pseudo_comments.
  ENDMETHOD.

  METHOD y_if_scan_statistics~get_number_warnings.
    result = number_warnings.
  ENDMETHOD.

  METHOD y_if_scan_statistics~increment_pseudo_comment_cnt.
    number_pseudo_comments = number_pseudo_comments + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_test_code_detector IMPLEMENTATION.
  METHOD y_if_testcode_detector~clear.
    CLEAR ref_scan_manager.
    CLEAR test_codes.
    CLEAR test_code.
    CLEAR statement_wa.
  ENDMETHOD.

  METHOD y_if_testcode_detector~set_ref_scan_manager.
    me->ref_scan_manager = ref_scan_manager.
  ENDMETHOD.

  METHOD y_if_testcode_detector~is_testcode.
    determine_test_code( ).
    IF test_codes IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE ref_scan_manager->get_statements( ) INTO statement_wa INDEX structure-stmnt_from.
    IF is_test_class( ).
      result = abap_true.

    ELSE.
      DATA(high_level_structure) = structure.

      DO.
        DATA(low_level_structure) = high_level_structure.
        READ TABLE ref_scan_manager->get_structures( ) INTO high_level_structure INDEX low_level_structure-back.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.

        READ TABLE ref_scan_manager->get_statements( ) INTO statement_wa INDEX high_level_structure-stmnt_from.
        IF is_test_class( ).
          result = abap_true.
          EXIT.
        ENDIF.
        IF low_level_structure-back EQ 0.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD is_test_class.
    IF keyword( ) = 'CLASS'.
      DATA(class) = get_token_rel( 2 ).
      READ TABLE test_codes TRANSPORTING NO FIELDS WITH KEY class = class.
      IF sy-subrc EQ 0.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD determine_test_code.
    IF test_codes IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition.

      process_statements( <structure> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD process_statements.
    CLEAR test_code.

    LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
      FROM structure-stmnt_from TO structure-stmnt_to.

      statement_wa = <statement>.
      process_tokens( <statement> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD process_tokens.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      IF testclass_added( <token> ).
        EXIT.
      ENDIF.

      IF testmethod_added( <token> ).
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD testclass_added.
    IF token-str EQ 'TESTING' AND
       keyword( ) = 'CLASS'.
      test_code-class = get_token_rel( 2 ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD testmethod_added.
    IF test_code-class IS NOT INITIAL AND (
        keyword( ) = 'METHODS' OR
        keyword( ) = 'CLASS-METHODS' ).

      test_code-method = get_token_rel( 2 ).
      APPEND test_code TO test_codes.
      result = abap_true.
    ENDIF.

    IF test_code-class IS NOT INITIAL AND
       test_code-method IS INITIAL AND
       keyword( ) = 'ENDCLASS'.
      APPEND test_code TO test_codes.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD keyword.
    IF statement_wa-type = 'C'.
      result = 'COMPUTE'.
      RETURN.
    ENDIF.
    READ TABLE ref_scan_manager->get_tokens( ) INDEX statement_wa-from INTO DATA(token_wa).
    result = token_wa-str.
  ENDMETHOD.

  METHOD get_token_rel.
    DATA(l_index) = statement_wa-from + p_n - 1.
    IF l_index > statement_wa-to.
      RETURN.
    ENDIF.
    READ TABLE ref_scan_manager->get_tokens( ) INDEX l_index INTO DATA(token_wa).
    result = token_wa-str.
  ENDMETHOD.
ENDCLASS.

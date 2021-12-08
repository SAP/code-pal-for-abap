*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltd_exemption IMPLEMENTATION.

  METHOD y_if_code_pal_exemption~is_exempt.
    result = abap_false.
  ENDMETHOD.

ENDCLASS.



CLASS ltd_creation_date IMPLEMENTATION.

  METHOD constructor.
    me->check = check.
  ENDMETHOD.

  METHOD y_if_code_pal_creation_date~get_creation_date.
    result = check->settings-object_created_on.
  ENDMETHOD.

ENDCLASS.



CLASS ltd_statistics IMPLEMENTATION.

  METHOD y_if_code_pal_statistics~collect.
    DATA(pseudo_comment) = is_pseudo_comment( ref_scan = ref_scan
                                              scimessages = scimessages
                                              test = test
                                              code = code
                                              suppress = suppress
                                              position = position ).

    IF pseudo_comment = cl_ci_test_root=>c_pc_exceptn_exists.
      count-pseudo_comments = count-pseudo_comments + 1.
    ELSEIF kind = y_code_pal_base=>c_error.
      count-errors = count-errors + 1.
    ELSEIF kind = y_code_pal_base=>c_warning.
      count-warnings = count-warnings + 1.
    ELSEIF kind = y_code_pal_base=>c_note.
      count-notes = count-notes + 1.
    ENDIF.
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
    ELSEIF suppress IS INITIAL.
      pcom = suppress.
    ELSEIF suppress CS ec_comment.
      pcom = suppress+5.
    ELSEIF suppress CS ec_prefix.
      pcom = suppress+4.
    ELSE.
      pcom = suppress.
    ENDIF.
  ENDMETHOD.


  METHOD has_comment.
    result = cl_ci_test_root=>c_pc_exceptn_posibl.

    DATA(l_position) = position + 1.

    DO.
      READ TABLE ref_scan->statements INTO DATA(l_statement_wa) INDEX l_position.
      IF sy-subrc <> 0 OR l_statement_wa-type <> 'P'.
        EXIT.
      ENDIF.

      LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<l_token_wa>)
        FROM l_statement_wa-from TO l_statement_wa-to.

        IF <l_token_wa>-str CS ec_comment AND (
              <l_token_wa>-str CS |{ ec_prefix } { pcom }| OR
              <l_token_wa>-str CS |{ ec_prefix } { pcom_alt }| OR
              <l_token_wa>-str CS |{ ec_prefix } *| ).
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
      READ TABLE ref_scan->statements INTO DATA(l_statement_wa) INDEX l_position.
      IF sy-subrc <> 0 OR ( l_statement_wa-type <> 'S' AND l_statement_wa-type <> 'G' ).
        EXIT.
      ENDIF.

      IF l_statement_wa-type = 'S'.
        LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<l_token_wa>)
          FROM l_statement_wa-from TO l_statement_wa-to.

          IF <l_token_wa>-str CS ec_comment AND (
                <l_token_wa>-str CS |{ ec_prefix } { pcom }| OR
                <l_token_wa>-str CS |{ ec_prefix } { pcom_alt }| OR
                <l_token_wa>-str CS |{ ec_prefix } *| ).
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


  METHOD is_pseudo_comment.
    determine_pseudo_comments( scimessages = scimessages
                               test        = test
                               code        = code
                               suppress    = suppress ).

    IF pcom = cl_ci_test_root=>c_exceptn_imposibl
    OR pcom IS INITIAL.
      RETURN.
    ENDIF.

    IF position IS INITIAL.
      RETURN.
    ENDIF.

    result = has_comment( ref_scan = ref_scan
                          position = position ).

    IF result <> cl_ci_test_root=>c_pc_exceptn_exists.
      result = has_inline_comment( ref_scan = ref_scan
                                   position = position ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.



CLASS ltd_scope IMPLEMENTATION.

  METHOD y_if_code_pal_scope~is_it_in_scope.
    result = abap_true.
  ENDMETHOD.

ENDCLASS.



CLASS ltd_profile IMPLEMENTATION.

  METHOD Y_IF_CODE_PAL_PROFILE~check_delegation_rights.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~check_time_overlap.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~cleanup_profile.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~create.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~delete_check.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~delete_delegate.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~delete_profile.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~delete_profiles.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~get_checks_from_db.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~get_check_description.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~get_registered_profiles.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~import_profile.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~insert_check.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~insert_delegate.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~insert_profile.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~mass_change.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~profile_exists.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~register_standard_profile.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~remove_all_checks.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~remove_all_delegates.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~select_all_profiles.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~select_checks.
    RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~select_delegates.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~select_existing_checks.
    RETURN.
  ENDMETHOD.


  METHOD Y_IF_CODE_PAL_PROFILE~select_profiles.
    RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
  ENDMETHOD.

ENDCLASS.

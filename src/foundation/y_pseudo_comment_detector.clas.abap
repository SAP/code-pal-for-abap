CLASS y_pseudo_comment_detector DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES: y_if_pseudo_comment_detector.
    ALIASES is_pseudo_comment FOR y_if_pseudo_comment_detector~is_pseudo_comment.

  PRIVATE SECTION.
    DATA pcom     TYPE sci_pcom.
    DATA pcom_alt TYPE sci_pcom.

    METHODS:
      determine_pseudo_comments
        IMPORTING
          !scimessages TYPE scimessages
          !test        TYPE sci_chk
          !code        TYPE sci_errc
          !suppress    TYPE sci_pcom,
      has_comment
        IMPORTING
          !ref_scan    TYPE REF TO cl_ci_scan
          !position    TYPE int4
        RETURNING
          VALUE(result) TYPE sci_pcom,
      has_inline_comment
        IMPORTING
          !ref_scan     TYPE REF TO cl_ci_scan
          !position     TYPE int4
        RETURNING
          VALUE(result) TYPE sci_pcom.
ENDCLASS.



CLASS y_pseudo_comment_detector IMPLEMENTATION.


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
    ELSEIF suppress CS y_if_pseudo_comment_detector=>ec_comment.
      pcom = suppress+5.
    ELSEIF suppress CS y_if_pseudo_comment_detector=>ec_prefix.
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

        IF <l_token_wa>-str CS y_if_pseudo_comment_detector=>ec_comment AND (
              <l_token_wa>-str CS |{ y_if_pseudo_comment_detector=>ec_prefix } { pcom }| OR
              <l_token_wa>-str CS |{ y_if_pseudo_comment_detector=>ec_prefix } { pcom_alt }| OR
              <l_token_wa>-str CS |{ y_if_pseudo_comment_detector=>ec_prefix } *| ).
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

          IF <l_token_wa>-str CS y_if_pseudo_comment_detector=>ec_comment AND (
                <l_token_wa>-str CS |{ y_if_pseudo_comment_detector=>ec_prefix } { pcom }| OR
                <l_token_wa>-str CS |{ y_if_pseudo_comment_detector=>ec_prefix } { pcom_alt }| OR
                <l_token_wa>-str CS |{ y_if_pseudo_comment_detector=>ec_prefix } *| ).
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

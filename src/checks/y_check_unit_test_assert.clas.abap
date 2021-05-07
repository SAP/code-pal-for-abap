CLASS y_check_unit_test_assert DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS get_parameter_reference IMPORTING statement TYPE sstmnt
                                              parameter TYPE string
                                    RETURNING VALUE(result) TYPE string
                                    RAISING cx_sy_itab_line_not_found.

    METHODS is_variable IMPORTING token TYPE stokesx
                        RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_check_unit_test_assert IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC UT_ASSERT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-apply_on_productive_code = abap_false.
    settings-apply_on_test_code = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }unit_test_assert.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-method ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Invalid Unit Test Assertion!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(token) = ref_scan_manager->tokens[ statement-from ].

    IF token-str NP '*ASSERT*'
    OR token-type = scan_token_type-comment.
      RETURN.
    ENDIF.

    TRY.
        DATA(act) = get_parameter_reference( statement = statement
                                             parameter = 'ACT' ).
        DATA(exp) = get_parameter_reference( statement = statement
                                             parameter = 'EXP' ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF act IS INITIAL
    OR exp IS INITIAL.
      RETURN.
    ENDIF.

    IF act <> exp.
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


  METHOD get_parameter_reference.
    DATA(in) = abap_false.
    DATA(depth) = 0.
    DATA(position) = statement-from.

    DO.
      IF position = statement-to.
        RETURN.
      ENDIF.

      DATA(token) = ref_scan_manager->tokens[ position ].

      IF token-type = scan_token_type-comment.
        position = position + 1.
        CONTINUE.
      ENDIF.

      IF token-str = parameter.
        in = abap_true.
        depth = 0.
        position = position + 2.
        CONTINUE.
      ENDIF.

      IF in = abap_false.
        position = position + 1.
        CONTINUE.
      ENDIF.

      IF token-str CP '*(*'.
        depth = depth + 1.
      ELSEIF token-str CP '*)*'.
        depth = depth - 1.
      ENDIF.

      IF depth = 0
      AND line_exists( ref_scan_manager->tokens[ position + 1 ] ).
        DATA(next) = ref_scan_manager->tokens[ position + 1 ].
        IF next-str = '='.
          in = abap_false.
          RETURN.
        ENDIF.
      ENDIF.

      IF is_variable( token ) = abap_false.
        token-str = '*'.
      ENDIF.

      result = COND #( WHEN result IS INITIAL THEN token-str
                                              ELSE |{ result } { token-str }| ).

      position = position + 1.
    ENDDO.
  ENDMETHOD.


  METHOD is_variable.
    CHECK token-type = scan_token_type-identifier.
    result = xsdbool( token-str CN '0123456789' ).
  ENDMETHOD.


ENDCLASS.

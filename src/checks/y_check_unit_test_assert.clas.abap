CLASS y_check_unit_test_assert DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS get_act_and_exp IMPORTING statement TYPE sstmnt
                            EXPORTING act TYPE stokesx
                                      exp TYPE stokesx.

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
    CHECK get_token_abs( statement-from ) CP 'CL_ABAP_UNIT_ASSERT=>ASSERT*'
    OR get_token_abs( statement-from ) CP 'CL_AUNIT_ASSERT=>ASSERT*'.

    get_act_and_exp( EXPORTING statement = statement
                     IMPORTING act = DATA(act)
                               exp = DATA(exp) ).

    IF act IS INITIAL
    OR exp IS INITIAL.
      RETURN.
    ENDIF.

    IF act-str <> exp-str
      AND ( is_variable( act ) = abap_true
        OR is_variable( exp ) = abap_true ).
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


  METHOD get_act_and_exp.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      DATA(tabix) = sy-tabix.
      CASE <token>-str.
        WHEN 'ACT'.
          act = ref_scan_manager->tokens[ tabix + 2 ].
        WHEN 'EXP'.
          exp = ref_scan_manager->tokens[ tabix + 2 ].
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_variable.
    result = COND #( WHEN token-type = scan_token_type-literal THEN abap_false
                     WHEN token-type = scan_token_type-identifier THEN xsdbool( token-str CN '0123456789' ) ).
  ENDMETHOD.


ENDCLASS.

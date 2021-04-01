CLASS y_check_unit_test_assert DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS get_act_and_exp IMPORTING statement TYPE sstmnt
                            EXPORTING act TYPE string
                                      exp TYPE string.

    METHODS is_variable IMPORTING statement TYPE sstmnt
                                  variable_name TYPE string
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS has_variable_in_scope IMPORTING structure TYPE sstruc
                                            variable_name TYPE string
                                  RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_check_unit_test_assert IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC UT_ASSERT' ##NO_variable_name.
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

    IF act <> exp.
      IF is_variable( variable_name = act statement = statement ) = abap_true
      OR is_variable( variable_name = exp statement = statement ) = abap_true.
        RETURN.
      ENDIF.
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
          act = get_token_abs( tabix + 2 ).
        WHEN 'EXP'.
          exp = get_token_abs( tabix + 2 ).
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_variable.
    CHECK variable_name CN '0123456789'.

    DATA(method) = ref_scan_manager->structures[ statement-struc ].

    result = has_variable_in_scope( structure = method
                                    variable_name = variable_name ).

    IF result = abap_true.
      RETURN.
    ENDIF.

    DATA(class_implementation) = ref_scan_manager->structures[ method-back ].
    DATA(class) = ref_scan_manager->structures[ class_implementation-back ].

    LOOP AT ref_scan_manager->structures INTO DATA(class_definition)
    FROM class-struc_from TO class-struc_to
    WHERE stmnt_type = scan_struc_stmnt_type-class_definition.
      result = has_variable_in_scope( structure = class_definition
                                      variable_name = variable_name ).
    ENDLOOP.
  ENDMETHOD.


  METHOD has_variable_in_scope.
    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
    FROM structure-stmnt_from TO structure-stmnt_to.
      LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
      FROM <statement>-from TO <statement>-to
      WHERE type <> scan_token_type-comment.
        IF <token>-str = |DATA({ variable_name })|.
          result = abap_true.
          RETURN.
        ELSEIF <token>-str = 'DATA'
        OR <token>-str = 'CLASS-DATA'.
          DATA(next_token) = ref_scan_manager->tokens[ sy-tabix + 1 ].
          IF next_token-str = variable_name.
            result = abap_true.
            RETURN.
          ENDIF.
        ELSEIF <token>-str = 'ENDCLASS'.
          RETURN.
        ENDIF.
      ENDLOOP.
   ENDLOOP.
  ENDMETHOD.


ENDCLASS.

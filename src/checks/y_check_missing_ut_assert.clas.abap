CLASS y_check_missing_ut_assert DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.

    METHODS is_testing_method
      IMPORTING method_name              TYPE string
      RETURNING VALUE(is_testing_method) TYPE abap_bool.
ENDCLASS.


CLASS y_check_missing_ut_assert IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-disable_threshold_selection   = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-apply_on_productive_code      = abap_false.
    settings-apply_on_test_code            = abap_true.
    settings-threshold                     = 0.
    settings-prio                          = c_warning.
    settings-documentation                 = |{ c_docs_path-checks }unit_test_missing_assert.md|.
    settings-pseudo_comment                = '"#EC CI_MISS_ASSERT'.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-method ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( 'Missing assertion in unit test method "&1"' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    " Check if method part of a class FOR TESTING
    CHECK is_test_code( statement ).

    " Start at each method
    CHECK get_token_abs( statement-from ) = 'METHOD'.

    " Check method on detail
    DATA(token_index) = statement-from.
    DATA(method_name) = get_token_abs( token_index + 1 ).

    " Check if method is marked as FOR TESTING
    IF NOT is_testing_method( method_name ).
      RETURN.
    ENDIF.

    " Check inside method
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
         FROM token_index.
      IF to_upper( <token>-str ) = 'ENDMETHOD'.
        EXIT.
      ENDIF.

      IF to_upper( <token>-str ) CP |CL_ABAP_UNIT_ASSERT=>*|.
        " If at least one ASSERT was found, all fine we can leave here.
        RETURN.
      ENDIF.
      token_index = token_index + 1.
    ENDLOOP.

    " Up to here now valid coding was found, so finding has to be raised.
    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 check_configuration = check_configuration
                 parameter_01        = |{ method_name }| ).

  ENDMETHOD.


  METHOD is_testing_method.

    IF method_name IS INITIAL.
      is_testing_method = abap_false.
      RETURN.
    ENDIF.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE str = 'METHODS'.
      " Safeguarding boundaries
      IF lines( ref_scan->tokens ) < sy-tabix + 3.
        CONTINUE.
      ENDIF.

      IF     ( to_upper( <token>-str ) = 'METHODS' )
         AND ( to_upper( ref_scan->tokens[ sy-tabix + 1 ]-str ) = method_name )
         AND ( to_upper( ref_scan->tokens[ sy-tabix + 2 ]-str ) = 'FOR' )
         AND ( to_upper( ref_scan->tokens[ sy-tabix + 3 ]-str ) = 'TESTING' ).
        is_testing_method = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    is_testing_method = abap_false.

  ENDMETHOD.

ENDCLASS.

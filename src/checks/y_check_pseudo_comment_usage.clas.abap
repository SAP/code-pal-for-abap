CLASS y_check_pseudo_comment_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_structures REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    

    CLASS-DATA class_names TYPE string_table.

    
    DATA pseudo_comment_counter TYPE i VALUE 0 ##NO_TEXT.

    METHODS count_cc_pseudo_comments IMPORTING token TYPE stokesx.

    METHODS check_result.

    METHODS call_get_pseudo_comment IMPORTING obj_name TYPE stokesx-str
                                    RETURNING VALUE(result) TYPE stokesx-str
                                    RAISING cx_sy_create_object_error.

ENDCLASS.



CLASS y_check_pseudo_comment_usage IMPLEMENTATION.


  METHOD call_get_pseudo_comment.
    DATA obj TYPE REF TO y_check_base.
    CREATE OBJECT obj TYPE (obj_name).
    result = obj->settings-pseudo_comment.
    IF result IS INITIAL.
      RAISE EXCEPTION TYPE cx_sy_create_object_error.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    has_documentation = abap_false.

    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_true.
    settings-apply_on_productive_code = abap_true.
    settings-ignore_pseudo_comments = abap_true.

    relevant_statement_types = VALUE #( BASE relevant_statement_types
                                      ( scan_struc_stmnt_type-class_definition )
                                      ( scan_struc_stmnt_type-interface ) ).

    set_check_message( '&1 pseudo comments!' ).
  ENDMETHOD.


  METHOD count_cc_pseudo_comments.
    LOOP AT class_names ASSIGNING FIELD-SYMBOL(<object_name>).
      TRY.
          IF token-str CS call_get_pseudo_comment( <object_name> ).
            pseudo_comment_counter = pseudo_comment_counter + 1.
          ENDIF.
        CATCH cx_sy_create_object_error.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_structures.
    pseudo_comment_counter = 0.

    TRY.
        IF class_names IS INITIAL.
          class_names = y_profile_manager=>get_checks_from_db( ).
        ENDIF.
      CATCH cx_failed.
        APPEND INITIAL LINE TO class_names.
    ENDTRY.

    super->inspect_structures( ).

    check_result( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE type = 'C'
    OR type = 'P'.
      count_cc_pseudo_comments( <token> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD check_result.
    CHECK pseudo_comment_counter > 0.

    DATA(statement) = ref_scan->statements[ 1 ].

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = statement-from
                 statement_from = statement-from
                 check_configuration = check_configuration
                 parameter_01 = |{ pseudo_comment_counter }| ).
  ENDMETHOD.


ENDCLASS.

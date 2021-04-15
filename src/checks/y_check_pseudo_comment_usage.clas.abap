CLASS y_check_pseudo_comment_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_structures REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS check_base_name TYPE tadir-obj_name VALUE 'Y_CHECK_BASE'.

    DATA name_tab TYPE STANDARD TABLE OF tadir-obj_name.
    DATA pseudo_comment_counter TYPE i VALUE 0 ##NO_TEXT.
    DATA class_names TYPE string_table.

    METHODS count_cc_pseudo_comments IMPORTING token TYPE stokesx.

    METHODS check_result.

    METHODS select_object_list RETURNING VALUE(result) LIKE name_tab
                               RAISING cx_failed.

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

    TRY.
        class_names = select_object_list( ).
      CATCH cx_failed.
        APPEND INITIAL LINE TO class_names.
    ENDTRY.
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

    super->inspect_structures( ).

    check_result( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE type = 'C'
    OR type = 'P'.
      count_cc_pseudo_comments( <token> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD check_result.
    CHECK pseudo_comment_counter > 0.

    DATA(check_configuration) = detect_check_configuration( VALUE #( level = 1 ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = 1
                 statement_index = 1
                 statement_from  = 1
                 error_priority = check_configuration-prio
                 parameter_01 = |{ pseudo_comment_counter }| ).
  ENDMETHOD.


  METHOD select_object_list.
    SELECT SINGLE devclass
    FROM tadir
    WHERE obj_name = @myname
    INTO @DATA(packagename).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    SELECT obj_name
    FROM tadir
    WHERE devclass = @packagename
    AND obj_name <> @check_base_name
    INTO TABLE @result.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

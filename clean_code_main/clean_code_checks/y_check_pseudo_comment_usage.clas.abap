CLASS y_check_pseudo_comment_usage DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.
    CONSTANTS check_base_name TYPE tadir-obj_name VALUE 'Y_CHECK_BASE'.
    DATA name_tab TYPE STANDARD TABLE OF tadir-obj_name.

    METHODS select_object_list
      RETURNING VALUE(result) LIKE name_tab
      RAISING   cx_failed.

    METHODS call_get_pseudo_comment
      IMPORTING obj_name      TYPE stokesx-str
      RETURNING VALUE(result) TYPE stokesx-str
      RAISING   cx_sy_create_object_error.

    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA pseudo_comment_counter TYPE i VALUE 0 ##NO_TEXT.
    DATA class_names TYPE string_table.

    METHODS count_cc_pseudo_comments
      IMPORTING token       TYPE stokesx
                class_names TYPE string_table.

    METHODS raise_error_wrapper.
ENDCLASS.



CLASS Y_CHECK_PSEUDO_COMMENT_USAGE IMPLEMENTATION.


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
    settings-prio = c_note.

    set_check_message( '[Clean Code]: &1 pseudo comments' ).
  ENDMETHOD.


  METHOD count_cc_pseudo_comments.
    LOOP AT class_names ASSIGNING FIELD-SYMBOL(<object_name>).
      TRY.
          IF token-str CS call_get_pseudo_comment( <object_name> ).
            ADD 1 TO pseudo_comment_counter.
          ENDIF.
        CATCH cx_sy_create_object_error.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD execute_check.
    TRY.
        class_names = select_object_list( ).
      CATCH cx_failed.
        APPEND INITIAL LINE TO class_names.
    ENDTRY.

    pseudo_comment_counter = 0.
    is_testcode = abap_false.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      WHERE type EQ 'C' OR type EQ 'P'.
      count_cc_pseudo_comments( token = <token> class_names = class_names ).
    ENDLOOP.

    raise_error_wrapper( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD raise_error_wrapper.
    IF pseudo_comment_counter GT 0.
      DATA(check_configuration) = detect_check_configuration( VALUE #( level = 1 ) ).
      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = 1
                   statement_index     = 1
                   statement_from      = 1
                   error_priority      = check_configuration-prio
                   parameter_01        = |{ pseudo_comment_counter }| ).
    ENDIF.
  ENDMETHOD.


  METHOD select_object_list.
    SELECT SINGLE devclass FROM tadir
      WHERE obj_name EQ @myname
      INTO @DATA(packagename).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    SELECT obj_name FROM tadir
      WHERE devclass EQ @packagename AND
            obj_name NE @check_base_name
      INTO TABLE @result.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

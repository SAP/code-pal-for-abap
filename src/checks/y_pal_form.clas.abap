CLASS y_pal_form DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS is_screen_event RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_PAL_FORM IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_FORM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }form-routine.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-form ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( '"FORM" Routine should not be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK keyword( ) = if_kaizen_keywords_c=>gc_form.
    CHECK is_screen_event( ) = abap_false.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD is_screen_event.
    DATA(form) = get_token_abs( statement_wa-from + 1 ).

    DATA(views) = manager->database_access->get_view_maintenance_routines( CONV #( form ) ).

    IF lines( views ) = 0.
      RETURN.
    ENDIF.

    LOOP AT views ASSIGNING FIELD-SYMBOL(<view>).
      LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS WHERE str = <view>-tabname. "#EC PREF_LINE_EX
        IF get_token_abs( sy-tabix - 1 ) = if_kaizen_keywords_c=>gc_tables.
          result = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

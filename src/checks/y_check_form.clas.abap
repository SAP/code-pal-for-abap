CLASS y_check_form DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_screen_event RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_CHECK_FORM IMPLEMENTATION.


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

    SELECT tabname
    FROM tvimf
    INTO TABLE @DATA(views)
    WHERE formname = @form.

    IF views IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT views ASSIGNING FIELD-SYMBOL(<view>).
      LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS WHERE str = <view>.
        IF get_token_abs( sy-tabix - 1 ) = if_kaizen_keywords_c=>gc_tables.
          result = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.

CLASS y_check_form DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS run REDEFINITION.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS is_screen_event RETURNING VALUE(result) TYPE abap_bool.
    METHODS apply_ignored_objecttypes.

ENDCLASS.



CLASS y_check_form IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_FORM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }form-routine.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-form ) ).
    relevant_structure_types = VALUE #( ).

    apply_ignored_objecttypes( ).

    set_check_message( '"FORM" Routine should not be used!' ).
  ENDMETHOD.

  method apply_ignored_objecttypes.
    " Do not run this check for adobe forms related objects
    APPEND VALUE #(
      sign  = 'E'
      option = 'EQ'
      low = objecttypes-from_object
    ) TO typelist.

    APPEND VALUE #(
      sign  = 'E'
      option = 'EQ'
      low = objecttypes-smart_form
    ) TO typelist.
  endmethod.


  METHOD inspect_tokens.
    CHECK keyword( ) = if_kaizen_keywords_c=>gc_form.
    CHECK is_screen_event( ) = abap_false.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
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

  METHOD run.
    " Do not run this check for adobe forms related objects
    " This should be done via injections to the typelist in the constructor
    " But is not doable without a sap note (see #558)
    " Remove this check as soon as a sap note is provided
    IF object_type = objecttypes-smart_form
      OR object_type = objecttypes-from_object.
      RETURN.
    ENDIF.
    super->run( ).
  ENDMETHOD.


ENDCLASS.
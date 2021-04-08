CLASS y_check_prefer_is_not DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

    METHODS is_standard_function IMPORTING token TYPE stokesx
                                 RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_check_prefer_is_not IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREFER_IS_NOT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-is-not-to-not-is.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-if ) ).
    relevant_structure_types = VALUE #(  ).

    set_check_message( 'Prefer IS NOT to NOT IS!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    LOOP AT ref_scan_manager->tokens TRANSPORTING NO FIELDS
    FROM statement-from TO statement-to
    WHERE str = 'IF'
    OR str = 'ELSEIF'
    OR str = 'AND'
    OR str = 'OR'
    OR str = 'ASSERT'.

      DATA(position) = sy-tabix.

      TRY.
          IF ref_scan_manager->tokens[ position + 1 ]-str <> 'NOT'.
            CONTINUE.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      TRY.
          IF is_standard_function( ref_scan_manager->tokens[ position + 2 ] ) = abap_true.
            CONTINUE.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      DATA(configuration) = detect_check_configuration( statement ).

      IF configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = configuration-prio ).

    ENDLOOP.
  ENDMETHOD.


  METHOD is_standard_function.
    result = xsdbool(    token-str CP 'CONTAINS*'
                      OR token-str CP 'LINE_EXISTS*'
                      OR token-str CP 'MATCHES*' ).
  ENDMETHOD.


ENDCLASS.

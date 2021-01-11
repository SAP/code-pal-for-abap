CLASS y_check_form DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

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
    CHECK get_token_abs( statement-from ) = 'FORM'.

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio ).
  ENDMETHOD.


ENDCLASS.

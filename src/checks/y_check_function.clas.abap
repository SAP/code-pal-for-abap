CLASS y_check_function DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA db_reader TYPE REF TO lif_db_reader.

ENDCLASS.



CLASS Y_CHECK_FUNCTION IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_FUNCTION' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }function-routine.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-function ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Function-Module should not be created!' ).

    IF db_reader IS NOT BOUND.
      db_reader = NEW lcl_db_reader( ).
    ENDIF.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK statement-type <> scan_stmnt_type-comment.
    CHECK statement-type <> scan_stmnt_type-comment_in_stmnt.
    CHECK statement-type <> scan_stmnt_type-pragma.

    CHECK get_token_abs( statement-from ) = 'FUNCTION'.

    DATA(fm_name) = get_token_abs( statement-from + 1 ).

    IF db_reader->is_fm_rfc_enabled( CONV #( fm_name ) ) = abap_false.

      DATA(check_configuration) = detect_check_configuration( statement ).

      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio ).

    ENDIF.
  ENDMETHOD.

ENDCLASS.

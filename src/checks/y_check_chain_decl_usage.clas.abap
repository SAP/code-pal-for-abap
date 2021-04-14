CLASS y_check_chain_decl_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA rows_with_colon TYPE STANDARD TABLE OF stmnt_crow.

    METHODS has_error_not_raised_yet IMPORTING statement TYPE sstmnt RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_CHECK_CHAIN_DECL_USAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CHAIN_DECL_USAG' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }chain-declaration-usage.md|.

    set_check_message( 'Do not chain up-front declarations!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK statement-colonrow IS NOT INITIAL.
    CHECK statement-terminator = ','.
    CHECK get_token_abs( statement-from ) = 'DATA'.
    CHECK has_error_not_raised_yet( statement ).

    APPEND statement-colonrow TO rows_with_colon.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = configuration-prio ).

  ENDMETHOD.


  METHOD has_error_not_raised_yet.
    result = xsdbool( NOT line_exists( rows_with_colon[ table_line = statement-colonrow ] ) ).
  ENDMETHOD.


ENDCLASS.

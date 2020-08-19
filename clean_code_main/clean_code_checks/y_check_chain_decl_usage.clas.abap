CLASS y_check_chain_decl_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    CONSTANTS threshold TYPE i VALUE 1.
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

    description = 'Chain Declarations Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '050'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CHAIN_DECL_USAG' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = threshold.
    settings-documentation = |{ c_docs_path-checks }chain-declaration-usage.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Do not chain up-front declarations!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK statement-colonrow IS NOT INITIAL.
    CHECK statement-terminator = ','.
    CHECK get_token_abs( statement-from ) = 'DATA'.
    CHECK has_error_not_raised_yet( statement ).

    APPEND statement-colonrow TO rows_with_colon.

    DATA(configuration) = detect_check_configuration( error_count = threshold
                                                      include = get_include( p_level = statement-level ) ).
    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = configuration-prio ).

  ENDMETHOD.


  METHOD has_error_not_raised_yet.
    IF NOT line_exists( rows_with_colon[ table_line = statement-colonrow ] ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

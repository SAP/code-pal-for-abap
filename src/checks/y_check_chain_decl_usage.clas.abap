CLASS y_check_chain_decl_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_raised_issues,
             include TYPE slevel-name,
             line_number_colon TYPE sstmnt-colonrow,
           END OF ty_raised_issues.

    DATA raised_issues TYPE TABLE OF ty_raised_issues.

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

    DATA(include) = get_include( p_level = statement-level ).

    DATA(raised_issue) = VALUE ty_raised_issues( include = include
                                                 line_number_colon = statement-colonrow ).

    IF line_exists( raised_issues[ include           = raised_issue-include
                                   line_number_colon = raised_issue-line_number_colon ] ).
      RETURN.
    ENDIF.

    APPEND raised_issue TO raised_issues.

    DATA(configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = configuration ).
  ENDMETHOD.


ENDCLASS.

CLASS y_check_default_key DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

ENDCLASS.



CLASS y_check_default_key IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC DEFAULT_KEY' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }avoid-default-key.md|.

    relevant_statement_types = VALUE #( BASE relevant_statement_types
                                      ( scan_struc_stmnt_type-class_definition ) ).

    set_check_message( 'Avoid DEFAULT KEY!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'DATA'
    OR get_token_abs( statement-from ) = 'CLASS-DATA'
    OR get_token_abs( statement-from ) = 'TYPES'.

    DATA(tokens) = condense_tokens( statement ).

    IF tokens NS ' TABLE '
    OR tokens NS ' DEFAULT KEY '.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


ENDCLASS.

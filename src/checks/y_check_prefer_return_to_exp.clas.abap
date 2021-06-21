CLASS y_check_prefer_return_to_exp DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS has_only_one_exporting IMPORTING statement TYPE sstmnt
                                   RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_check_prefer_return_to_exp IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREFER_RET' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-returning-to-exporting.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-interface ) ).

    set_check_message( 'Prefer RETURNING to EXPORTING!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHODS'
    OR get_token_abs( statement-from ) = 'CLASS-METHODS'.

    CHECK has_only_one_exporting( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = configuration-prio ).
  ENDMETHOD.


  METHOD has_only_one_exporting.
    DATA(skip) = abap_false.
    DATA(count) = 0.

    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from
    TO statement-to.

      IF <token>-str = 'IMPORTING'
      OR <token>-str = 'CHANGING'
      OR <token>-str = 'RETURNING'.
        skip = abap_true.
      ELSEIF <token>-str = 'EXPORTING'.
        skip = abap_false.
      ENDIF.

      IF skip = abap_true.
        CONTINUE.
      ENDIF.

      IF <token>-str = 'TYPE'
      OR <token>-str = 'LIKE'.
        count = count + 1.
      ENDIF.

    ENDLOOP.

    result = xsdbool( count = 1 ).
  ENDMETHOD.


ENDCLASS.
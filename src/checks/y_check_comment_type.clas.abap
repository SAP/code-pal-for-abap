CLASS y_check_comment_type DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    METHODS has_wrong_comment_type IMPORTING statement TYPE sstmnt RETURNING VALUE(result) TYPE abap_bool.


ENDCLASS.



CLASS y_check_comment_type IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-disable_threshold_selection = abap_true.
    settings-ignore_pseudo_comments = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }comment-type.md|.

    set_check_message( 'Comment with ", not with *!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK statement-type = 'P'.
    CHECK has_wrong_comment_type( statement ).

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD has_wrong_comment_type.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE type = scan_token_type-comment.
      TRY.
          IF has_token_started_with( token = <token>-str
                                     start_with = |*"| )
              OR has_token_started_with( token = <token>-str
                                         start_with = |*&| ).
            CONTINUE.
          ENDIF.

          IF has_token_started_with( token = <token>-str
                                     start_with = |*| ).
            result = abap_true.
            RETURN.
          ENDIF.
        CATCH cx_sy_range_out_of_bounds.
          result = abap_false.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.

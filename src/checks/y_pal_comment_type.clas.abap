CLASS y_pal_comment_type DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.
  PRIVATE SECTION.
    METHODS has_wrong_comment_type IMPORTING statement TYPE sstmnt RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_first_character IMPORTING token TYPE stokesx RETURNING VALUE(result) TYPE char1.
    METHODS get_second_character IMPORTING token TYPE stokesx RETURNING VALUE(result) TYPE char1.
ENDCLASS.



CLASS y_pal_comment_type IMPLEMENTATION.


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
    FROM statement-from TO statement-to.
      IF get_first_character( <token> ) = '*'
      AND get_second_character( <token> ) <> '&'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_first_character.
    TRY.
        result = token-str(1).
      CATCH cx_sy_range_out_of_bounds.
        result = ''.
    ENDTRY.
  ENDMETHOD.


  METHOD get_second_character.
    TRY.
        result = token-str(2).
      CATCH cx_sy_range_out_of_bounds.
        result = ''.
    ENDTRY.
  ENDMETHOD.


  METHOD add_check_quickfix.
    " Comments are not supported
    RETURN.
  ENDMETHOD.

ENDCLASS.

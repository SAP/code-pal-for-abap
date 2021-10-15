CLASS y_check_prefer_pragmas DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF mapping,
             pragma TYPE slin_desc-pragma,
             pseudo_com TYPE slin_desc-pseudo_com,
           END OF mapping.

    CLASS-DATA mappings TYPE TABLE OF mapping.

    METHODS extract_pseudo_comment IMPORTING statement TYPE sstmnt
                                   RETURNING value(result)  TYPE string.

    METHODS to_pragma IMPORTING pseudo_comment TYPE string
                      RETURNING value(result)  TYPE string.

ENDCLASS.



CLASS y_check_prefer_pragmas IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-ignore_pseudo_comments = abap_true.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-pragmas-to-pseudo-comments.md|.

    IF mappings IS INITIAL.
      SELECT pragma, pseudo_com FROM slin_desc INTO CORRESPONDING FIELDS OF TABLE @mappings.
    ENDIF.

    set_check_message( 'Change the &1 to &2' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(pseudo_comment) = extract_pseudo_comment( statement ).

    IF pseudo_comment IS INITIAL.
      RETURN.
    ENDIF.

    DATA(pragma) = to_pragma( pseudo_comment ).

    IF pragma IS INITIAL.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    pragma = |##{ pragma }|.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration
                 parameter_01 = pseudo_comment
                 parameter_02 = pragma ).
  ENDMETHOD.


  METHOD extract_pseudo_comment.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE type = scan_token_type-comment.
      IF <token>-str CS '"#EC'.
        result = <token>-str.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD to_pragma.
    TRY.
        DATA(text) = pseudo_comment.
        REPLACE '"#EC' IN text WITH ''.
        CONDENSE text.
        result = mappings[ pseudo_com = text ]-pragma.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS y_pal_prefer_pragmas DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF mapping,
             pragma TYPE slin_desc-pragma,
             pseudo_com TYPE slin_desc-pseudo_com,
           END OF mapping.

    TYPES pseudo_comments TYPE SORTED TABLE OF mapping-pseudo_com WITH UNIQUE KEY table_line.

    CLASS-DATA mappings TYPE TABLE OF mapping.

    METHODS get_pseudo_comments IMPORTING statement TYPE sstmnt
                                RETURNING VALUE(result) TYPE pseudo_comments.

    METHODS get_mapping IMPORTING pseudo_comments TYPE pseudo_comments
                        RETURNING VALUE(result) LIKE mappings.

ENDCLASS.



CLASS y_pal_prefer_pragmas IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-ignore_pseudo_comments = abap_true.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-pragmas-to-pseudo-comments.md|.

    IF mappings IS INITIAL.
      SELECT DISTINCT pragma, pseudo_com FROM slin_desc INTO CORRESPONDING FIELDS OF TABLE @mappings.
    ENDIF.

    set_check_message( 'Change the &1 to &2' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(pseudo_comments) = get_pseudo_comments( statement ).

    IF lines( pseudo_comments ) = 0.
      RETURN.
    ENDIF.

    DATA(mapping_to_pragmas) = get_mapping( pseudo_comments ).

    IF lines( mapping_to_pragmas ) = 0.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    LOOP AT mapping_to_pragmas ASSIGNING FIELD-SYMBOL(<mapping>).
      raise_error( statement_level = statement-level
                   statement_index = index
                   statement_from = statement-from
                   check_configuration = check_configuration
                   parameter_01 = |#EC { <mapping>-pseudo_com }|
                   parameter_02 = |##{ <mapping>-pragma }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_pseudo_comments.
    LOOP AT ref_scan->tokens INTO token_wa
    FROM statement-from TO statement-to
    WHERE type = scan_token_type-comment
    AND str CS '#EC '.
      REPLACE ALL OCCURRENCES OF `"#EC ` IN token_wa-str WITH `#EC `.
      REPLACE ALL OCCURRENCES OF ` #EC` IN token_wa-str WITH '#EC'.
      REPLACE ALL OCCURRENCES OF `#EC ` IN token_wa-str WITH '#EC'.
      SPLIT token_wa-str AT space INTO TABLE DATA(pseudo_comments).
      LOOP AT pseudo_comments ASSIGNING FIELD-SYMBOL(<pseudo_comment>)
      WHERE table_line CS '#EC'.
        REPLACE ALL OCCURRENCES OF '#EC' IN <pseudo_comment> WITH ''.
        INSERT CONV #( <pseudo_comment> ) INTO TABLE result.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_mapping.
    result = FILTER #( mappings IN pseudo_comments WHERE pseudo_com = table_line ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

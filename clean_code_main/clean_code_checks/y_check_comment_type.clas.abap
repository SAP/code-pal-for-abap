CLASS y_check_comment_type DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    METHODS has_wrong_comment_type IMPORTING statement TYPE sstmnt RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_first_character IMPORTING token TYPE stokesx RETURNING VALUE(result) TYPE char1.
ENDCLASS.

CLASS y_check_comment_type IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }comment-type.md|.

    set_check_message( '[Clean Code]: Comment with ", not with *!' ).
  ENDMETHOD.

  METHOD inspect_tokens.

    CHECK statement-type = 'P'.
    CHECK has_wrong_comment_type( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).

  ENDMETHOD.

  METHOD has_wrong_comment_type.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF get_first_character( <token> ) = '*'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_first_character.
    result = token-str(1).
  ENDMETHOD.

ENDCLASS.

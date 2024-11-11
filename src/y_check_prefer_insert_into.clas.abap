CLASS y_check_prefer_insert_into DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS: inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS y_check_prefer_insert_into IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREF_INSERT_INT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-insert-into-to-append.md|.

    set_check_message( 'Prefer INSERT INTO TABLE to APPEND TO.' ).

  ENDMETHOD.

  METHOD inspect_tokens.

    IF get_token_abs( statement-from ) <> 'APPEND'.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).

  ENDMETHOD.

ENDCLASS.

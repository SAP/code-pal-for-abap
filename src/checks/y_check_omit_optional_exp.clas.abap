CLASS y_check_omit_optional_exp DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS has_optional_exporting IMPORTING statement TYPE sstmnt
                                   RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS y_check_omit_optional_exp IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC OPTL_EXP' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }omit-optional-exporting.md|.

    set_check_message( 'Omit the optional keyword EXPORTING!' ).
  ENDMETHOD.

  METHOD inspect_tokens.

    CHECK statement-type = 'A'
       OR statement-type = 'C'.

    CHECK has_optional_exporting( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).

  ENDMETHOD.

  METHOD has_optional_exporting.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str = 'EXPORTING'.
          result = abap_true.
      ELSEIF <token>-str = 'IMPORTING'
      OR <token>-str = 'CHANGING'
      OR <token>-str = 'RECEIVING'
      OR <token>-str = 'EXCEPTIONS'.
          result = abap_false.
          RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS y_check_optl_exporting_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    METHODS has_optional_exporting IMPORTING statement TYPE sstmnt
                                   RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS Y_CHECK_OPTL_EXPORTING_USAGE IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    description = 'Optional Keyword EXPORTING Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '090'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC OPTL_EXP' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }optional-exporting-usage.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Omit the optional keyword EXPORTING!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
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
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      IF <token>-str = 'EXPORTING'.
          result = abap_true.
      ELSEIF <token>-str = 'IMPORTING'
      OR <token>-str = 'CHANGING'
      OR <token>-str = 'RECEIVING'.
          result = abap_false.
          RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

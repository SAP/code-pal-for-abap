CLASS y_check_non_class_exception DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_NON_CLASS_EXCEPTION' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS checkif_error
      IMPORTING index TYPE i.
ENDCLASS.



CLASS Y_CHECK_NON_CLASS_EXCEPTION IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                            include = get_include( p_level = statement_for_message-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( p_sub_obj_type = c_type_include
                 p_level        = statement_for_message-level
                 p_position     = index
                 p_from         = statement_for_message-from
                 p_kind         = check_configuration-prio
                 p_test         = me->myname
                 p_code         = get_code( check_configuration-prio ) ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    description = 'Non-class-based Exception Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '510'.
    version = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC NON_CL_EXCEPT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }non-class-exception.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Non-class-based exceptions should not be used!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD inspect_tokens.
    statement_for_message = statement.
    CASE get_token_abs( statement-from ).
      WHEN 'RAISE'.
        IF 'EXCEPTION RESUMABLE SHORTDUMP EVENT' NS get_token_abs( statement-from + 1 ).
          checkif_error( index ).
        ENDIF.

      WHEN 'MESSAGE'.
        LOOP AT ref_scan_manager->get_tokens( ) TRANSPORTING NO FIELDS
          FROM statement-from TO statement-to WHERE str = 'RAISING' AND type EQ 'I'.

          checkif_error( index ).
        ENDLOOP.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS y_pal_non_class_exception DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

    METHODS inspect_message IMPORTING statement TYPE sstmnt
                                      index     TYPE i.

    METHODS inspect_raise IMPORTING statement TYPE sstmnt
                                    index     TYPE i.

  PRIVATE SECTION.
    METHODS checkif_error IMPORTING index     TYPE i
                                    statement TYPE sstmnt.

ENDCLASS.


CLASS Y_PAL_NON_CLASS_EXCEPTION IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NON_CL_EXCEPT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }non-class-exception.md|.

    set_check_message( 'Non-class-based exceptions should not be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    inspect_raise( statement = statement
                   index = index ).

    inspect_message( statement = statement
                     index = index ).
  ENDMETHOD.


  METHOD inspect_message.
    CHECK get_token_abs( statement-from ) = 'MESSAGE'.

    LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS
    FROM statement-from TO statement-to
    WHERE str = 'RAISING' AND type = 'I'.
      checkif_error( index = index
                     statement = statement ).

    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_raise.
    CHECK get_token_abs( statement-from ) = 'RAISE'.
    CHECK statement-from + 1 = statement-to.

    checkif_error( index = index
                   statement = statement ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

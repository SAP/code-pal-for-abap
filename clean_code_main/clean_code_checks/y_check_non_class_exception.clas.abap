CLASS y_check_non_class_exception DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS checkif_error
      IMPORTING index     TYPE i
                statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_NON_CLASS_EXCEPTION IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( statement ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NON_CL_EXCEPT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }non-class-exception.md|.

    set_check_message( '[Clean Code]: Non-class-based exceptions should not be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CASE get_token_abs( statement-from ).
      WHEN 'RAISE'.
        IF 'RESUMABLE SHORTDUMP EVENT' NS get_token_abs( statement-from + 1 ) AND NOT
          ( get_token_abs( statement-from + 1 ) EQ 'EXCEPTION' AND get_token_abs( statement-from + 2 ) EQ 'TYPE' ).
          checkif_error( index = index
                         statement = statement ).
        ENDIF.
      WHEN 'MESSAGE'.
        LOOP AT ref_scan_manager->get_tokens( ) TRANSPORTING NO FIELDS
          FROM statement-from TO statement-to WHERE str = 'RAISING' AND type EQ 'I'.
          checkif_error( index = index
                         statement = statement ).
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

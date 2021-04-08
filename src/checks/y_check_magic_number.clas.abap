CLASS y_check_magic_number DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS second_token TYPE i VALUE 2 ##NO_TEXT.

    DATA magic_number TYPE string.
    DATA has_case_with_subrc TYPE abap_bool.

    METHODS is_exception IMPORTING token        TYPE string
                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_keyword_valid RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_magic_number IMPORTING token_string TYPE string
                            RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_CHECK_MAGIC_NUMBER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_MAGIC' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_false.
    settings-documentation = |{ c_docs_path-checks }magic-number.md|.

    set_check_message( 'Magic Number Violation - &1 is a Magic Number!' ).
  ENDMETHOD.


  METHOD is_exception.
    IF token = 'ENDCASE' AND has_case_with_subrc = abap_true.
      has_case_with_subrc = abap_false.
    ENDIF.

    IF token = 'SY-SUBRC' OR has_case_with_subrc = abap_true.
      result = abap_true.
    ELSEIF token = 'CASE' AND get_token_rel( second_token ) = 'SY-SUBRC'.
      has_case_with_subrc = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD inspect_tokens.
    statement_wa = statement.

    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.

      IF is_exception( <token>-str ) = abap_true.
        EXIT.
      ENDIF.

      IF is_magic_number( <token>-str ).
        DATA(check_configuration) = detect_check_configuration( statement_wa ). "#EC DECL_IN_IF
        IF check_configuration IS INITIAL.
          CONTINUE.
        ENDIF.

        raise_error( statement_level     = statement_wa-level
                     statement_index     = index
                     statement_from      = statement_wa-from
                     error_priority      = check_configuration-prio
                     parameter_01        = |{ magic_number }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_keyword_valid.
    DATA(keyword) = keyword( ).
    result = xsdbool( keyword = 'DO' OR
                      keyword = 'IF' OR
                      keyword = 'ELSEIF' OR
                      keyword = 'WHEN' OR
                      keyword = 'CHECK' ).
  ENDMETHOD.


  METHOD is_magic_number.
    IF is_keyword_valid( ) = abap_false.
      RETURN.
    ENDIF.

    FIND REGEX `^(?!'?[01]'?$)'?\d+'?$` IN token_string.
    IF sy-subrc = 0.
      magic_number = token_string.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


ENDCLASS.

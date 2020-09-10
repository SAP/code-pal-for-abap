CLASS y_check_magic_number DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS second_token TYPE i VALUE 2 ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS inspect_tokens
        REDEFINITION .
  PRIVATE SECTION.

    DATA magic_number TYPE string .
    DATA token_excluded TYPE abap_bool .

    METHODS exclude_token
      IMPORTING
        !token        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS is_keyword_valid
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS is_magic_number
      IMPORTING
        !token_string TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool .
ENDCLASS.



CLASS Y_CHECK_MAGIC_NUMBER IMPLEMENTATION.


  METHOD constructor .
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_MAGIC' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_false.
    settings-documentation = |{ c_docs_path-checks }magic-number.md|.

    set_check_message( '[Clean Code]: Magic Number Violation - &1 is a Magic Number' ).
  ENDMETHOD.


  METHOD exclude_token.
    IF token EQ 'ENDCASE' AND token_excluded EQ abap_true.
      token_excluded = abap_false.
    ENDIF.

    IF token EQ 'SY-SUBRC' OR token_excluded EQ abap_true.
      result = abap_true.
    ELSEIF token EQ 'CASE' AND get_token_rel( second_token ) = 'SY-SUBRC'.
      token_excluded = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD inspect_tokens.
    statement_wa = statement.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      IF exclude_token( <token>-str ) = abap_true.
        EXIT.
      ENDIF.

      IF is_magic_number( <token>-str ).
        DATA(check_configuration) = detect_check_configuration( statement_wa ).
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

    FIND REGEX '^(?![01]$)\d*$' IN token_string.
    IF sy-subrc EQ 0.
      magic_number = token_string.
      result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

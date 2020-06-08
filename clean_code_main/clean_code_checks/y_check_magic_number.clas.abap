CLASS y_check_magic_number DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE seoclsname VALUE 'Y_CHECK_MAGIC_NUMBER' ##NO_TEXT.
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

    description = 'Magic Number Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '420'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CI_MAGIC' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_false.
    settings-documentation = |{ c_docs_path-checks }magic-number.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Magic Number Violation - &1 is a Magic Number'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
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
        DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                                include = get_include( p_level = statement_wa-level ) ).
        IF check_configuration IS INITIAL.
          CONTINUE.
        ENDIF.

        raise_error( p_sub_obj_type = c_type_include
                     p_level        = statement_wa-level
                     p_position     = index
                     p_from         = statement_wa-from
                     p_kind         = check_configuration-prio
                     p_test         = me->myname
                     p_code         = get_code( check_configuration-prio )
                     p_param_1      = |{ magic_number }| ).
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

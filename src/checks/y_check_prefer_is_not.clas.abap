CLASS y_check_prefer_is_not DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

    METHODS is_standard_function
      IMPORTING
        token         TYPE stokesx
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_predicative_method
      IMPORTING
        !tokens         TYPE stokesx_tab
        !statement      TYPE sstmnt
        position_of_not TYPE syst_tabix
      RETURNING
        VALUE(result)   TYPE abap_bool.

  PRIVATE SECTION.
    METHODS get_position_closing_bracket
      IMPORTING
        tokens                   TYPE stokesx_tab
        statement                TYPE sstmnt
        position_of_open_bracket TYPE syst_tabix
      RETURNING
        VALUE(result)            TYPE i.

ENDCLASS.



CLASS y_check_prefer_is_not IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREFER_IS_NOT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-is-not-to-not-is.md|.

    set_check_message( 'Prefer IS NOT to NOT IS!' ).
  ENDMETHOD.

  METHOD inspect_tokens.
    LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS
         FROM statement-from TO statement-to
         WHERE    str = 'IF'
               OR str = 'ELSEIF'
               OR str = 'AND'
               OR str = 'OR'
               OR str = 'ASSERT'
               or str = 'CHECK'.

      DATA(position) = sy-tabix.

      TRY.
          IF ref_scan->tokens[ position + 1 ]-str <> 'NOT'.
            CONTINUE.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      TRY.
          IF is_standard_function( ref_scan->tokens[ position + 2 ] ).
            CONTINUE.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      IF is_predicative_method( tokens          = ref_scan->tokens
                                statement       = statement
                                position_of_not = position + 1 ).
        CONTINUE.
      ENDIF.

      DATA(check_configuration) = detect_check_configuration( statement ).

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   check_configuration = check_configuration ).
    ENDLOOP.
  ENDMETHOD.

  METHOD is_standard_function.
    result = xsdbool(    token-str CP 'CONTAINS*'
                      OR token-str CP 'LINE_EXISTS*'
                      OR token-str CP 'MATCHES*' ).
  ENDMETHOD.

  METHOD is_predicative_method.
    DATA(position_to_check) = position_of_not + 1.

    IF tokens[ position_to_check ]-str = 'NEW'.
      position_to_check = position_to_check + 1.
    ENDIF.

    IF tokens[ position_to_check ]-str NP '*('.
      " This is not the start of a method call
      RETURN.
    ENDIF.

    DATA(position_closing_bracket) = get_position_closing_bracket(
                                         tokens                   = tokens
                                         statement                = statement
                                         position_of_open_bracket = position_to_check ).

    IF position_closing_bracket = statement-to.
      " Nothing follows the closing bracket
      result = abap_true.
      RETURN.
    ENDIF.

    DATA(next_string) = tokens[ position_closing_bracket + 1 ]-str.

    IF next_string = 'AND' OR next_string = 'OR'.
      " Nothing follows until the next condition starts
      result = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD get_position_closing_bracket.
    DATA(open_brackets) = 1.
    result = position_of_open_bracket.

    WHILE open_brackets <> 0 AND result <= statement-to.
      result = result + 1.
      DATA(current_string) = tokens[ result ]-str.

      IF current_string CP '*('.
        open_brackets = open_brackets + 1.
      ENDIF.

      IF current_string CP ')*'.
        open_brackets = open_brackets - 1.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
CLASS y_check_deprecated_key_words DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS check_if_error IMPORTING index     TYPE i
                                     keyword   TYPE string
                                     statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_DEPRECATED_KEY_WORDS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC DEPRECATED_KEY' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }deprecated-key-word.md|.

    set_check_message( '"&1" is deprecated!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(keyword) = get_token_abs( statement-from ).
    CASE keyword.
      WHEN 'MOVE' OR 'TRANSLATE'.
        check_if_error( index   = index
                        keyword = keyword
                        statement = statement ).
    ENDCASE.
  ENDMETHOD.


  METHOD check_if_error.
    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ keyword }| ).
  ENDMETHOD.
ENDCLASS.

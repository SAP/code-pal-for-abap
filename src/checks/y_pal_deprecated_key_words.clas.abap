CLASS y_pal_deprecated_key_words DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS check_if_error IMPORTING index     TYPE i
                                     keyword   TYPE string
                                     statement TYPE sstmnt.
ENDCLASS.



CLASS y_pal_deprecated_key_words IMPLEMENTATION.

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

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration
                 parameter_01 = |{ keyword }| ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

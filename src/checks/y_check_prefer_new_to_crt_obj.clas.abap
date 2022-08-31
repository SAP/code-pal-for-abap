CLASS y_check_prefer_new_to_crt_obj DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

ENDCLASS.



CLASS y_check_prefer_new_to_crt_obj IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREF_NEW' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-new-to-create-object.md|.

    set_check_message( 'Prefer NEW to CREATE OBJECT!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(has_keyword) = xsdbool(
      get_token_abs( statement-from ) =  'CREATE' AND
      get_token_abs( statement-from + 1 ) = 'OBJECT' ).
    DATA(is_rap_testing_call) = xsdbool(
      get_token_abs( statement-to - 1 ) = 'FOR' AND
      get_token_abs( statement-to ) = 'TESTING' ).
    DATA(is_ole_call) = COND #(
      WHEN statement-from + 3 <= statement-to
        THEN COND abap_bool(
          LET fourth_token = get_token_abs( statement-from + 3 ) IN
          WHEN fourth_token(1) = `'` THEN abap_true ELSE abap_false
        )
        ELSE abap_false ).
    DATA(has_dynamic_typing) = xsdbool( next1( 'TYPE' ) CA '()' ).

    IF has_keyword = abap_true AND
       is_rap_testing_call = abap_false AND
       is_ole_call = abap_false AND
       has_dynamic_typing = abap_false.
      DATA(check_configuration) = detect_check_configuration( statement ).

      raise_error( statement_level = statement-level
                   statement_index = index
                   statement_from = statement-from
                   check_configuration = check_configuration ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.

CLASS y_pal_message_easy_to_find DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS is_message_dynamic IMPORTING statement TYPE sstmnt
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_message_class_explicitly RETURNING VALUE(result) TYPE string.
    METHODS is_text_element RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_sy_msgid RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_static_short_form RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_method_call RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_pal_message_easy_to_find IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC MSG_FIND'.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }message-easy-to-find.md|.

    set_check_message( 'Make the Message Easy to Find!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK statement-type = scan_stmnt_type-standard.
    CHECK get_token_abs( statement-from ) = 'MESSAGE'.

    "TODO: Recursive
    IF ref_scan->structures[ statement-struc ]-stmnt_type = scan_struc_stmnt_type-catch.
      RETURN.
    ENDIF.

    IF is_message_dynamic( statement ) = abap_false.
      RETURN.
    ENDIF.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD is_message_dynamic.
    token_wa = ref_scan->tokens[ statement-from + 1 ].

    IF token_wa-str = 'ID'.
      token_wa = ref_scan->tokens[ statement-from + 2 ].

      IF token_wa-type = scan_token_type-literal.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF token_wa-type = scan_token_type-literal.
      RETURN.
    ENDIF.

    IF is_method_call( ) = abap_true
    OR is_text_element( ) = abap_true
    OR is_sy_msgid( ) = abap_true
    OR is_static_short_form( ) = abap_true
    OR is_message_class_explicitly( ) = abap_true.
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.


  METHOD is_message_class_explicitly.
    DATA(message_class) = token_wa-str.

    IF token_wa-str CA ( '(' )
    AND token_wa-str CA ( ')' ).
      SPLIT token_wa-str AT '(' INTO TABLE DATA(parts).
      SPLIT parts[ 2 ] AT ')' INTO TABLE parts.
      message_class = parts[ 1 ].
    ENDIF.

    DATA(metadata) = manager->database_access->get_message_class( CONV #( message_class ) ).

    result = xsdbool( lines( metadata ) > 0 ).
  ENDMETHOD.


  METHOD is_text_element.
    result = xsdbool( contains( val = token_wa-str
                                start = 'TEXT-' ) ).
  ENDMETHOD.


  METHOD is_sy_msgid.
    result = xsdbool( token_wa-str = 'SY-MSGID' ).
  ENDMETHOD.


  METHOD is_static_short_form.
    CONSTANTS size_of_short_form TYPE i VALUE 4.
    CONSTANTS message_types TYPE string VALUE 'AEISW'.
    CHECK strlen( token_wa-str ) = size_of_short_form.
    CHECK token_wa-str(1) CA message_types.
    CHECK token_wa-str+1(3) NA sy-abcde.
    result = abap_true.
  ENDMETHOD.


  METHOD is_method_call.
    result = xsdbool( token_wa-str CA '=>'
                   OR token_wa-str CA '->' ).
  ENDMETHOD.

  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

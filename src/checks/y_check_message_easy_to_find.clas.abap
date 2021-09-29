CLASS y_check_message_easy_to_find DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA message_classes TYPE HASHED TABLE OF arbgb WITH UNIQUE KEY table_line.

    METHODS load_message_classes.

    METHODS is_message_dynamic IMPORTING statement TYPE sstmnt
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_message_class_explicitly IMPORTING string TYPE string
                                        RETURNING VALUE(result) TYPE string.

    METHODS is_text_element IMPORTING string TYPE string
                            RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_sy_msgid IMPORTING string TYPE string
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_static_short_form IMPORTING string TYPE string
                                 RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_check_message_easy_to_find IMPLEMENTATION.

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
    DATA(token) = ref_scan->tokens[ statement-from + 1 ].

    IF token-str = 'ID'.
      token = ref_scan->tokens[ statement-from + 2 ].

      IF token-type = scan_token_type-literal.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF token-type = scan_token_type-literal.
      RETURN.
    ENDIF.

    IF is_text_element( token-str ) = abap_true
    OR is_sy_msgid( token-str ) = abap_true
    OR is_static_short_form( token-str ) = abap_true
    OR is_message_class_explicitly( token-str ) = abap_true.
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.


  METHOD is_message_class_explicitly.
    DATA(message_class) = string.

    IF string CA ( '()' ).
      SPLIT string AT '(' INTO TABLE DATA(parts).
      SPLIT parts[ 2 ] AT ')' INTO TABLE parts.
      message_class = parts[ 1 ].
    ENDIF.

    load_message_classes( ).

    result = xsdbool( line_exists( message_classes[ table_line = message_class ] ) ).
  ENDMETHOD.


  METHOD is_text_element.
    result = xsdbool( contains( val = string
                                start = 'TEXT-' ) ).
  ENDMETHOD.


  METHOD load_message_classes.
    CHECK message_classes IS INITIAL.

    SELECT arbgb
    FROM t100a
    INTO TABLE @message_classes.
  ENDMETHOD.


  METHOD is_sy_msgid.
    result = xsdbool( string = 'SY-MSGID' ).
  ENDMETHOD.


  METHOD is_static_short_form.
    CONSTANTS size_of_short_form TYPE i VALUE 4.
    CHECK strlen( string ) = size_of_short_form.
    CHECK string(1) CA 'AEISW'.
    CHECK string+1(3) NA sy-abcde.
    result = abap_true.
  ENDMETHOD.

ENDCLASS.

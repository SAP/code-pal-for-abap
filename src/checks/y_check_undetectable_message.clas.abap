CLASS y_check_undetectable_message DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA message_classes TYPE HASHED TABLE OF arbgb WITH UNIQUE KEY table_line.

    METHODS load_message_classes.

    METHODS is_message_dynamic IMPORTING statement TYPE sstmnt
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_message_class_name IMPORTING string TYPE string
                                       RETURNING VALUE(result) TYPE string
                                       RAISING cx_sy_itab_line_not_found.

    METHODS is_message_class IMPORTING name TYPE string
                             RETURNING VALUE(result) TYPE string.

ENDCLASS.


CLASS y_check_undetectable_message IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC DYNAMIC_MSG'.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }undetectable-message-statement.md|.

    set_check_message( 'Make the Messages Easy to Find!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK statement-type = scan_stmnt_type-standard.
    CHECK get_token_abs( statement-from ) = 'MESSAGE'.

    IF ref_scan_manager->structures[ statement-struc ]-stmnt_type = scan_struc_stmnt_type-catch.
      RETURN.
    ENDIF.

    IF is_message_dynamic( statement ) = abap_false.
      RETURN.
    ENDIF.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).
  ENDMETHOD.


  METHOD is_message_dynamic.
    DATA(token) = ref_scan_manager->tokens[ statement-from + 1 ].

    IF token-str = 'ID'.
      token = ref_scan_manager->tokens[ statement-from + 2 ].

      IF token-type = scan_token_type-literal.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(message_class) = get_message_class_name( token-str ).

    IF message_class = 'SY-MSGID'
    OR is_message_class( message_class ) = abap_true.
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.


  METHOD get_message_class_name.
    DATA(contains_open_braket) = xsdbool( contains( val = string
                                                    sub = '(' ) ).

    DATA(contains_close_braket) = xsdbool( contains( val = string
                                                     sub = ')' ) ).

    IF contains_open_braket = abap_false
    OR contains_close_braket = abap_false.
      result = string.
      RETURN.
    ENDIF.

    SPLIT string AT '(' INTO TABLE DATA(parts).
    SPLIT parts[ 2 ] AT ')' INTO TABLE parts.
    result = parts[ 1 ].
  ENDMETHOD.


  METHOD is_message_class.
    IF strlen( name ) = 4.
      DATA(type) = name(1).
      DATA(id) = name+1(3).

      IF id CA sy-abcde
      AND type = 'A'
      OR type = 'E'
      OR type = 'I'
      OR type = 'S'
      OR type = 'W'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    load_message_classes( ).

    result = xsdbool( line_exists( message_classes[ table_line = name ] ) ).
  ENDMETHOD.


  METHOD load_message_classes.
    CHECK message_classes IS INITIAL.

    SELECT arbgb
    FROM t100a
    INTO TABLE @message_classes.
  ENDMETHOD.


ENDCLASS.

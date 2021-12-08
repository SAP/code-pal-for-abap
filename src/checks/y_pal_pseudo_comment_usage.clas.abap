CLASS y_pal_pseudo_comment_usage DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    TYPES ty_pseudo_comments LIKE SORTED TABLE OF settings-pseudo_comment WITH NON-UNIQUE KEY table_line.
    TYPES ty_string_table TYPE TABLE OF string.

    CLASS-DATA relevant_pseudo_comments TYPE ty_pseudo_comments.

    DATA counter TYPE i.

    METHODS get_relevant_pseudo_comments RETURNING VALUE(result) TYPE ty_pseudo_comments.

    METHODS get_count IMPORTING pseudo_comments TYPE ty_string_table
                      RETURNING VALUE(result) TYPE i.

    METHODS create_check IMPORTING name TYPE tadir-obj_name
                         RETURNING VALUE(result) TYPE REF TO y_code_pal_base.

ENDCLASS.



CLASS y_pal_pseudo_comment_usage IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    has_documentation = abap_false.

    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_true.
    settings-apply_on_productive_code = abap_true.
    settings-ignore_pseudo_comments = abap_true.

    relevant_statement_types = VALUE #( BASE relevant_statement_types
                                      ( scan_struc_stmnt_type-class_definition )
                                      ( scan_struc_stmnt_type-interface ) ).

    set_check_message( '&1 pseudo comments!' ).
  ENDMETHOD.


  METHOD execute_check.
    CLEAR counter.

    super->execute_check( ).

    IF counter = 0.
      RETURN.
    ENDIF.

    DATA(statement) = ref_scan->statements[ 1 ].

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = statement-from
                 statement_from = statement-from
                 check_configuration = check_configuration
                 parameter_01 = |{ counter }| ).
  ENDMETHOD.


  METHOD inspect_tokens.
    LOOP AT ref_scan->tokens INTO token_wa
    FROM statement-from TO statement-to
    WHERE type = scan_token_type-comment
    AND str CS '#EC '.
      REPLACE ALL OCCURRENCES OF `"#EC` IN token_wa-str WITH '#EC'.
      REPLACE ALL OCCURRENCES OF `#EC ` IN token_wa-str WITH '#EC'.
      SPLIT token_wa-str AT space INTO TABLE DATA(pseudo_comments).
      counter = counter + get_count( pseudo_comments ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_count.
    IF relevant_pseudo_comments IS INITIAL.
      relevant_pseudo_comments = get_relevant_pseudo_comments( ).
    ENDIF.

    LOOP AT pseudo_comments INTO DATA(pseudo_comment)
    WHERE table_line CS '#EC'.
      REPLACE ALL OCCURRENCES OF '#EC' IN pseudo_comment WITH ''.
      IF line_exists( relevant_pseudo_comments[ table_line = pseudo_comment ] ).
        result = result + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_relevant_pseudo_comments.
    LOOP AT y_code_pal_profile=>get_checks_from_db( ) ASSIGNING FIELD-SYMBOL(<check>)
    WHERE object = 'CLAS'.
      DATA(check) = create_check( <check>-obj_name ).
      IF check->settings-ignore_pseudo_comments = abap_true.
        CONTINUE.
      ENDIF.
      IF check->settings-pseudo_comment IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF `"#EC ` IN check->settings-pseudo_comment WITH ''.
        INSERT check->settings-pseudo_comment INTO TABLE result.
      ENDIF.
      IF check->settings-alternative_pseudo_comment IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF `"#EC ` IN check->settings-alternative_pseudo_comment WITH ''.
        INSERT check->settings-alternative_pseudo_comment INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_check.
    CREATE OBJECT result TYPE (name).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

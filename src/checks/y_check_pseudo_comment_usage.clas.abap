CLASS y_check_pseudo_comment_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_structures REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA pseudo_comments LIKE TABLE OF settings-pseudo_comment.
    DATA pseudo_comment_counter TYPE i.

    METHODS get_pseudo_comments RETURNING VALUE(result) LIKE pseudo_comments.
    METHODS count_pseudo_comments IMPORTING token TYPE stokesx.
    METHODS check_result.

ENDCLASS.



CLASS y_check_pseudo_comment_usage IMPLEMENTATION.


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


  METHOD inspect_structures.
    pseudo_comment_counter = 0.

    IF pseudo_comments IS INITIAL.
      pseudo_comments = get_pseudo_comments( ).
    ENDIF.

    super->inspect_structures( ).

    check_result( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE type = 'C'
    OR type = 'P'.
      count_pseudo_comments( <token> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_pseudo_comments.
    DATA(checks) = y_profile_manager=>get_checks_from_db( ).

    LOOP AT checks ASSIGNING FIELD-SYMBOL(<check>) WHERE object = 'CLAS'.
      DATA check TYPE REF TO y_check_base.
      CREATE OBJECT check TYPE (<check>-obj_name).
      IF check->settings-ignore_pseudo_comments = abap_false.
        APPEND check->settings-pseudo_comment TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD count_pseudo_comments.
    LOOP AT pseudo_comments ASSIGNING FIELD-SYMBOL(<pseudo_comment>).
      IF token-str CS <pseudo_comment>.
        pseudo_comment_counter = pseudo_comment_counter + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_result.
    CHECK pseudo_comment_counter > 0.

    DATA(statement) = ref_scan->statements[ 1 ].

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = statement-from
                 statement_from = statement-from
                 check_configuration = check_configuration
                 parameter_01 = |{ pseudo_comment_counter }| ).
  ENDMETHOD.


ENDCLASS.

CLASS y_check_function DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.

    DATA db_reader TYPE REF TO lif_db_reader .
ENDCLASS.



CLASS Y_CHECK_FUNCTION IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'FUNCTION Module Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '360'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CI_FUNCTION' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }function-routine.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Function-Module should not be created!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) EQ 'FUNCTION'.

    DATA fm_name TYPE c LENGTH 30.
    fm_name = get_token_abs( statement-from + 1 ).

    IF db_reader->is_fm_rfc_enabled( fm_name ) EQ abap_false.
      DATA(check_configuration) = detect_check_configuration( error_count = 0
                                                              include = get_include( p_level = statement-level ) ).
      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = structure-stmnt_from
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio ).
    ENDIF.
  ENDMETHOD.


  METHOD execute_check.
    IF db_reader IS NOT BOUND.
      db_reader = NEW lcl_db_reader( ).
    ENDIF.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-function.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_config) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to WHERE type NE scan_stmnt_type-comment AND
                                                                  type NE scan_stmnt_type-comment_in_stmnt AND
                                                                  type NE scan_stmnt_type-pragma.
        inspect_tokens( structure = <structure> statement = <statement> ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

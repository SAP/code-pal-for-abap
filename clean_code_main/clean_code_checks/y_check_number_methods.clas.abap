CLASS y_check_number_methods DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS execute_check
        REDEFINITION .
    METHODS inspect_tokens
        REDEFINITION .
  PRIVATE SECTION.
    DATA method_counter TYPE i VALUE 0.

    METHODS checkif_error
      IMPORTING index     TYPE i
                statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_NUMBER_METHODS IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( threshold = method_counter
                                                            include = get_include( p_level = statement-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF method_counter > check_configuration-threshold.
      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio
                   parameter_01        = |{ method_counter }|
                   parameter_02        = |{ check_configuration-threshold }| ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    description = 'Number of Methods'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '660'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC NUMBER_METHODS' ##NO_TEXT.
    settings-threshold = 20.
    settings-documentation = |{ c_docs_path-checks }number-methods.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: &1 methods, exceeding threshold of &2'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition OR
            stmnt_type EQ scan_struc_stmnt_type-interface.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      READ TABLE ref_scan_manager->get_statements( ) INTO DATA(statement_for_message)
        INDEX <structure>-stmnt_from.
      method_counter = 0.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( statement = <statement> ).
      ENDLOOP.

      checkif_error( index = <structure>-stmnt_from
                     statement = statement_for_message ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CASE get_token_abs( statement-from ).
      WHEN 'METHODS' OR 'CLASS-METHODS'.
        ADD 1 TO method_counter.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

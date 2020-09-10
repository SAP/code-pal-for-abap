CLASS y_check_number_interfaces DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA interface_counter TYPE i VALUE 0.

    METHODS checkif_error
      IMPORTING index     TYPE i
                statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_NUMBER_INTERFACES IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( error_count = interface_counter
                                                            statement = statement ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level      = statement-level
                 statement_index      = index
                 statement_from       = statement-from
                 error_priority       = check_configuration-prio
                 parameter_01         = |{ interface_counter }|
                 parameter_02         = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NMBR_INTERFACES' ##NO_TEXT.
    settings-threshold = 4.
    settings-documentation = |{ c_docs_path-checks }number-interfaces.md|.

    set_check_message( '[Clean Code]: There are &1 interfaces, exceeding threshold of &2' ).
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
      interface_counter = 0.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( statement = <statement> ).
      ENDLOOP.

      checkif_error( index = <structure>-stmnt_from
                     statement = statement_for_message ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) EQ 'INTERFACES'.
    ADD 1 TO interface_counter.
  ENDMETHOD.
ENDCLASS.

CLASS y_check_number_attributes DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS execute_check REDEFINITION.

  PRIVATE SECTION.
    DATA attribute_counter TYPE i VALUE 0.
    DATA structure_depth TYPE i VALUE 0.
    CONSTANTS structure_depth_threshold TYPE i VALUE 0.

    METHODS checkif_attribute_in_structure
      IMPORTING
        second_token TYPE string
        third_token  TYPE string.

    METHODS checkif_attribute_found
      IMPORTING first_token TYPE string.

    METHODS checkif_error
      IMPORTING index TYPE i
                statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_NUMBER_ATTRIBUTES IMPLEMENTATION.


  METHOD checkif_attribute_found.
    CASE first_token.
      WHEN 'DATA' OR 'CLASS-DATA'.
        IF structure_depth <= structure_depth_threshold.
          ADD 1 TO attribute_counter.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD checkif_attribute_in_structure.
    IF ( second_token = 'BEGIN' AND third_token = 'OF' ).
      ADD 1 TO structure_depth.
    ELSEIF ( second_token = 'END' AND third_token = 'OF' ).
      SUBTRACT 1 FROM structure_depth.
    ENDIF.
  ENDMETHOD.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( error_count = attribute_counter
                                                            statement = statement ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ attribute_counter }|
                 parameter_02        = |{ check_configuration-threshold }| ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    description = 'Number of Attributes'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '540'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC NUMBER_ATTR' ##NO_TEXT.
    settings-threshold = 12.
    settings-documentation = |{ c_docs_path-checks }number-attributes.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: &1 attributes, exceeding threshold &2'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition
         OR stmnt_type EQ scan_struc_stmnt_type-interface.

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
      attribute_counter = 0.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( statement = <statement> ).
      ENDLOOP.

      checkif_error( index = <structure>-stmnt_from
                     statement = statement_for_message ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    checkif_attribute_found( first_token = get_token_abs( statement-from ) ).

    checkif_attribute_in_structure( second_token = get_token_abs( statement-from + 1 )
                                    third_token = get_token_abs( statement-from + 2 ) ).
  ENDMETHOD.
ENDCLASS.

CLASS y_check_num_public_attributes DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens
        REDEFINITION.

    METHODS execute_check
        REDEFINITION.

  PRIVATE SECTION.

    DATA public_attribute_counter TYPE i VALUE 0.
    DATA structure_depth TYPE i VALUE 0.
    CONSTANTS structure_depth_threshold TYPE i VALUE 0.

    METHODS checkif_attribute_in_structure
      IMPORTING
        second_token TYPE string
        third_token  TYPE string.

    METHODS checkif_public_attribute_found
      IMPORTING
        first_token TYPE string
        last_token  TYPE string.

    METHODS checkif_error
      IMPORTING index     TYPE i
                statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_NUM_PUBLIC_ATTRIBUTES IMPLEMENTATION.


  METHOD checkif_attribute_in_structure.
    IF ( second_token = 'BEGIN' AND third_token = 'OF' ).
      ADD 1 TO structure_depth.
    ELSEIF ( second_token = 'END' AND third_token = 'OF' ).
      SUBTRACT 1 FROM structure_depth.
    ENDIF.
  ENDMETHOD.


  METHOD checkif_error.

    DATA(check_configuration) = detect_check_configuration( statement = statement
                                                            error_count = public_attribute_counter ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ public_attribute_counter }|
                 parameter_02        = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD checkif_public_attribute_found.
    IF ( first_token = 'DATA' OR first_token = 'CLASS-DATA' )
       AND structure_depth <= structure_depth_threshold
       AND NOT last_token = 'READ-ONLY'.
      ADD 1 TO public_attribute_counter.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NUM_PUBLIC_ATTR' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 1.
    settings-documentation = |{ c_docs_path-checks }number-public-attributes.md|.

    set_check_message( '&1 public attributes. All attributes should be private/protected by default.' ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition.

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
      public_attribute_counter = 0.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        IF get_token_abs( <statement>-from ) EQ 'PROTECTED' OR
           get_token_abs( <statement>-from ) EQ 'PRIVATE'.
          EXIT.
        ENDIF.

        inspect_tokens( statement = <statement> ).
      ENDLOOP.

      checkif_error( index = <structure>-stmnt_from
                     statement = statement_for_message ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    checkif_public_attribute_found( first_token = get_token_abs( statement-from )
                                     last_token = get_token_abs( statement-to ) ).

    checkif_attribute_in_structure( second_token = get_token_abs( statement-from + 1 )
                                     third_token = get_token_abs( statement-from + 2 ) ).
  ENDMETHOD.
ENDCLASS.

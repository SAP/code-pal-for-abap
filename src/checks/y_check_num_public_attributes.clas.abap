CLASS y_check_num_public_attributes DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS execute_check REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS structure_depth_threshold TYPE i VALUE 0.

    DATA public_attribute_counter TYPE i VALUE 0.
    DATA structure_depth TYPE i VALUE 0.
    DATA leading_structure TYPE sstruc.

    METHODS checkif_attribute_in_structure IMPORTING second_token TYPE string
                                                     third_token  TYPE string.

    METHODS checkif_public_attribute_found IMPORTING first_token TYPE string
                                                     last_token  TYPE string.

    METHODS set_leading_structure IMPORTING structure TYPE sstruc.

    METHODS check_leading_structure.

ENDCLASS.



CLASS y_check_num_public_attributes IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NUM_PUBLIC_ATTR' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 1.
    settings-documentation = |{ c_docs_path-checks }number-public-attributes.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-public_section ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( '&1 public attributes. All attributes should be private/protected by default!' ).
  ENDMETHOD.


  METHOD execute_check.
    super->execute_check( ).
    check_leading_structure( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    IF leading_structure <> structure.
      check_leading_structure( ).
      set_leading_structure( structure ).
    ENDIF.

    checkif_public_attribute_found( first_token = get_token_abs( statement-from )
                                     last_token = get_token_abs( statement-to ) ).

    checkif_attribute_in_structure( second_token = get_token_abs( statement-from + 1 )
                                     third_token = get_token_abs( statement-from + 2 ) ).
  ENDMETHOD.


  METHOD checkif_attribute_in_structure.
    IF ( second_token = 'BEGIN' AND third_token = 'OF' ).
      ADD 1 TO structure_depth.
    ELSEIF ( second_token = 'END' AND third_token = 'OF' ).
      SUBTRACT 1 FROM structure_depth.
    ENDIF.
  ENDMETHOD.


  METHOD check_leading_structure.
    CHECK leading_structure IS NOT INITIAL.

    DATA(statement) = ref_scan_manager->statements[ leading_structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( statement = statement
                                                            error_count = public_attribute_counter ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = leading_structure-stmnt_from
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ public_attribute_counter }|
                 parameter_02        = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD checkif_public_attribute_found.
    CHECK first_token = 'DATA'
       OR first_token = 'CLASS-DATA'.

    CHECK structure_depth <= structure_depth_threshold.

    CHECK last_token <> 'READ-ONLY'.

    ADD 1 TO public_attribute_counter.
  ENDMETHOD.


  METHOD set_leading_structure.
    leading_structure = structure.
    public_attribute_counter = 0.
  ENDMETHOD.


ENDCLASS.

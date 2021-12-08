CLASS y_pal_num_public_attributes DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS structure_depth_threshold TYPE i VALUE 0.

    DATA public_attribute_counter TYPE i VALUE 0.
    DATA structure_depth TYPE i VALUE 0.

    METHODS checkif_attribute_in_structure IMPORTING second_token TYPE string
                                                     third_token  TYPE string.

    METHODS checkif_public_attribute_found IMPORTING first_token TYPE string
                                                     last_token  TYPE string.

    METHODS check_result IMPORTING structure TYPE sstruc.

ENDCLASS.



CLASS y_pal_num_public_attributes IMPLEMENTATION.


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


  METHOD inspect_statements.
    public_attribute_counter = 0.

    super->inspect_statements( structure ).

    check_result( structure ).
  ENDMETHOD.


  METHOD inspect_tokens.
    checkif_public_attribute_found( first_token = get_token_abs( statement-from )
                                     last_token = get_token_abs( statement-to ) ).

    checkif_attribute_in_structure( second_token = get_token_abs( statement-from + 1 )
                                     third_token = get_token_abs( statement-from + 2 ) ).
  ENDMETHOD.


  METHOD checkif_attribute_in_structure.
    IF second_token = 'BEGIN'
    AND third_token = 'OF'.
      structure_depth = structure_depth + 1.
    ELSEIF second_token = 'END'
    AND third_token = 'OF'.
      structure_depth = structure_depth - 1.
    ENDIF.
  ENDMETHOD.


  METHOD checkif_public_attribute_found.
    CHECK first_token = 'DATA'
       OR first_token = 'CLASS-DATA'.

    CHECK structure_depth <= structure_depth_threshold.

    CHECK last_token <> 'READ-ONLY'.

    public_attribute_counter = public_attribute_counter + 1.
  ENDMETHOD.


  METHOD check_result.
    DATA(statement) = ref_scan->statements[ structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( statement = statement
                                                            error_count = public_attribute_counter ).

    raise_error( statement_level     = statement-level
                 statement_index     = structure-stmnt_from
                 statement_from      = statement-from
                 check_configuration = check_configuration
                 parameter_01 = |{ public_attribute_counter }|
                 parameter_02 = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

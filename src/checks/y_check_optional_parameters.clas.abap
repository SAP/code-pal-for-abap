CLASS y_check_optional_parameters DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS has_optional_parameter IMPORTING statement TYPE sstmnt
                                   RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS Y_CHECK_OPTIONAL_PARAMETERS IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC OPTL_PARAM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }optional-parameters.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition ) ).
    relevant_structure_types = VALUE #(  ).

    set_check_message( 'Split methods instead of adding OPTIONAL parameters!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHODS'.
    CHECK has_optional_parameter( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).
  ENDMETHOD.

  METHOD has_optional_parameter.
    LOOP AT ref_scan_manager->tokens TRANSPORTING NO FIELDS
    FROM statement-from TO statement-to
    WHERE str = 'OPTIONAL'.
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

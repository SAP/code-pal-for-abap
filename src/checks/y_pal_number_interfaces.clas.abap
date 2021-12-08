CLASS y_pal_number_interfaces DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA interface_counter TYPE i VALUE 0.

    METHODS check_result IMPORTING structure TYPE sstruc.

ENDCLASS.



CLASS y_pal_number_interfaces IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NMBR_INTERFACES' ##NO_TEXT.
    settings-threshold = 4.
    settings-documentation = |{ c_docs_path-checks }number-interfaces.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-interface ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( 'Number of interfaces must be lower than &2! (&1>=&2)' ).
  ENDMETHOD.


  METHOD inspect_statements.
    interface_counter = 0.

    super->inspect_statements( structure ).

    check_result( structure ).
  ENDMETHOD.


  METHOD inspect_tokens.
    IF get_token_abs( statement-from ) = 'INTERFACES'.
      interface_counter = interface_counter + 1.
    ENDIF.
  ENDMETHOD.


  METHOD check_result.
    DATA(statement) = ref_scan->statements[ structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( error_count = interface_counter
                                                            statement = statement ).

    raise_error( statement_level = statement-level
                 statement_index = structure-stmnt_from
                 statement_from = statement-from
                 check_configuration = check_configuration
                 parameter_01 = |{ interface_counter }|
                 parameter_02 = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

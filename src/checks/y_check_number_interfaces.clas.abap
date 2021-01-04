CLASS y_check_number_interfaces DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA interface_counter TYPE i VALUE 0.
    DATA leading_structure TYPE sstruc.

    METHODS set_leading_structure IMPORTING structure TYPE sstruc.

    METHODS check_leading_structure.

ENDCLASS.



CLASS Y_CHECK_NUMBER_INTERFACES IMPLEMENTATION.


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


  METHOD execute_check.
    super->execute_check( ).
    check_leading_structure( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    IF leading_structure <> structure.
      check_leading_structure( ).
      set_leading_structure( structure ).
    ENDIF.

    IF get_token_abs( statement-from ) = 'INTERFACES'.
      ADD 1 TO interface_counter.
    ENDIF.
  ENDMETHOD.


  METHOD check_leading_structure.
    CHECK leading_structure IS NOT INITIAL.

    DATA(statement) = ref_scan_manager->statements[ leading_structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( error_count = interface_counter
                                                            statement = statement ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level      = statement-level
                 statement_index      = leading_structure-stmnt_from
                 statement_from       = statement-from
                 error_priority       = check_configuration-prio
                 parameter_01         = |{ interface_counter }|
                 parameter_02         = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD set_leading_structure.
    leading_structure = structure.
    interface_counter = 0.
  ENDMETHOD.


ENDCLASS.

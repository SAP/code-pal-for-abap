CLASS y_check_number_events DEFINITION PUBLIC INHERITING FROM y_check_base  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA event_counter TYPE i VALUE 0.

    METHODS check_result IMPORTING structure TYPE sstruc.

ENDCLASS.



CLASS Y_CHECK_NUMBER_EVENTS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NUMBER_EVENTS' ##NO_TEXT.
    settings-documentation = |{ c_docs_path-checks }number-events.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-interface ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( 'Number of events must be lower than &2! (&1>=&2)' ).
  ENDMETHOD.


  METHOD inspect_statements.
    event_counter = 0.

    super->inspect_statements( structure ).

    check_result( structure ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CASE get_token_abs( statement-from ).
      WHEN 'EVENTS' OR 'CLASS-EVENTS'.
        event_counter = event_counter + 1.
    ENDCASE.
  ENDMETHOD.


  METHOD check_result.
    DATA(statement) = ref_scan_manager->statements[ structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( error_count = event_counter
                                                            statement = statement ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = structure-stmnt_from
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ event_counter }|
                 parameter_02        = |{ check_configuration-threshold }| ).
  ENDMETHOD.


ENDCLASS.

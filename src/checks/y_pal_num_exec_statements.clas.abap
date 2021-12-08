CLASS y_pal_num_exec_statements DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA no_exec_statements TYPE i.
    DATA statement_for_message TYPE sstmnt.

    METHODS count_statements  CHANGING no_exec_statements TYPE i.

    METHODS determine_position IMPORTING type         TYPE flag
                                         index        TYPE i
                               RETURNING  VALUE(result) TYPE int4.

    METHODS is_keyword_exempt RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_statement_exempt IMPORTING type         TYPE stmnt_type
                                RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_PAL_NUM_EXEC_STATEMENTS IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_NOES' ##NO_TEXT.
    settings-threshold = 40.
    settings-documentation = |{ c_docs_path-checks }number-executable-statements.md|.

    add_obj_type( c_type_program ).

    set_check_message( 'Number of executable statements must be lower than &2! (&1>=&2)' ).
  ENDMETHOD.


  METHOD count_statements.
    IF is_statement_exempt( statement_wa-type ).
      RETURN.
    ENDIF.
    IF is_keyword_exempt( ).
      RETURN.
    ENDIF.
    IF token_wa-type <> scan_token_type-comment AND token_wa-type <> scan_token_type-pragma.
      no_exec_statements = no_exec_statements + 1.
    ENDIF.
  ENDMETHOD.


  METHOD determine_position.
    result = index.
    IF type = scan_struc_type-event.
      result = result - 1.
    ENDIF.
  ENDMETHOD.


  METHOD inspect_tokens.
    statement_wa = statement.

    IF index = structure-stmnt_from.
      statement_for_message = statement.
      no_exec_statements = 0.
    ENDIF.

    count_statements( CHANGING no_exec_statements = no_exec_statements ).

    IF index = structure-stmnt_to.
      DATA(check_configuration) = detect_check_configuration( error_count = no_exec_statements
                                                              statement = statement_for_message ).

      raise_error( statement_level = statement_for_message-level
                   statement_index = determine_position( type = structure-type index = index )
                   statement_from = statement_for_message-from
                   check_configuration = check_configuration
                   parameter_01 = |{ no_exec_statements }|
                   parameter_02 = |{ check_configuration-threshold }| ).
    ENDIF.
  ENDMETHOD.


  METHOD is_keyword_exempt.
    CASE keyword( ).
      WHEN if_kaizen_keywords_c=>gc_program
        OR if_kaizen_keywords_c=>gc_endclass
        OR if_kaizen_keywords_c=>gc_endif
        OR if_kaizen_keywords_c=>gc_endwhile
        OR if_kaizen_keywords_c=>gc_endloop
        OR if_kaizen_keywords_c=>gc_enddo
        OR if_kaizen_keywords_c=>gc_endtry
        OR if_kaizen_keywords_c=>gc_endselect
        OR if_kaizen_keywords_c=>gc_endcase
        OR if_kaizen_keywords_c=>gc_endat
        OR if_kaizen_keywords_c=>gc_endprovide
        OR if_kaizen_keywords_c=>gc_report
        OR if_kaizen_keywords_c=>gc_function_pool
        OR if_kaizen_keywords_c=>gc_class_pool
        OR if_kaizen_keywords_c=>gc_interface_pool
        OR if_kaizen_keywords_c=>gc_type_pool
        OR if_kaizen_keywords_c=>gc_data
        OR if_kaizen_keywords_c=>gc_field_symbols
        OR if_kaizen_keywords_c=>gc_types
        OR if_kaizen_keywords_c=>gc_class_data
        OR if_kaizen_keywords_c=>gc_constants
        OR if_kaizen_keywords_c=>gc_statics
        OR if_kaizen_keywords_c=>gc_tables
        OR if_kaizen_keywords_c=>gc_nodes
        OR if_kaizen_keywords_c=>gc_field_groups
        OR if_kaizen_keywords_c=>gc_define
        OR if_kaizen_keywords_c=>gc_end_of_definition
        OR if_kaizen_keywords_c=>gc_include
        OR if_kaizen_keywords_c=>gc_parameters
        OR if_kaizen_keywords_c=>gc_parameter
        OR if_kaizen_keywords_c=>gc_select_options
        OR if_kaizen_keywords_c=>gc_function
        OR if_kaizen_keywords_c=>gc_endfunction
        OR if_kaizen_keywords_c=>gc_method
        OR if_kaizen_keywords_c=>gc_endmethod
        OR if_kaizen_keywords_c=>gc_form
        OR if_kaizen_keywords_c=>gc_endform
        OR if_kaizen_keywords_c=>gc_module
        OR if_kaizen_keywords_c=>gc_endmodule
        OR if_kaizen_keywords_c=>gc_methods
        OR if_kaizen_keywords_c=>gc_class_methods
        OR if_kaizen_keywords_c=>gc_events
        OR if_kaizen_keywords_c=>gc_class_events
        OR if_kaizen_keywords_c=>gc_interfaces
        OR if_kaizen_keywords_c=>gc_aliases
        OR if_kaizen_keywords_c=>gc_syntax_trace
        OR if_kaizen_keywords_c=>gc_enhancement_point
        OR if_kaizen_keywords_c=>gc_load_of_program
        OR if_kaizen_keywords_c=>gc_initialization
        OR if_kaizen_keywords_c=>gc_start_of_selection
        OR if_kaizen_keywords_c=>gc_end_of_selection
        OR if_kaizen_keywords_c=>gc_top_of_page
        OR if_kaizen_keywords_c=>gc_end_of_page
        OR if_kaizen_keywords_c=>gc_user_command
        OR if_kaizen_keywords_c=>gc_line_selection
        OR if_kaizen_keywords_c=>gc_selection_screen.
        result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD is_statement_exempt.
    CONSTANTS:
      include_program           TYPE stmnt_type VALUE 'I',
      include_program_not_exist TYPE stmnt_type VALUE 'J',
      macro_definition          TYPE stmnt_type VALUE 'M',
      type_pools                TYPE stmnt_type VALUE 'T',
      type_pool_not_exist       TYPE stmnt_type VALUE 'V',
      comment_between_statement TYPE stmnt_type VALUE 'P',
      comment_in_statement      TYPE stmnt_type VALUE 'S',
      blank_statement           TYPE stmnt_type VALUE 'N'.

    IF type = include_program OR
       type = include_program_not_exist OR
       type = macro_definition OR
       type = type_pools OR
       type = type_pool_not_exist OR
       type = comment_between_statement OR
       type = comment_in_statement OR
       type = blank_statement.

      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.

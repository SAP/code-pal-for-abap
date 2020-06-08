CLASS y_check_num_exec_statements DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_NUM_EXEC_STATEMENTS' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS inspect_tokens
        REDEFINITION .

  PRIVATE SECTION.

    DATA no_exec_statements TYPE i .

    METHODS count_statements
      CHANGING
        !no_exec_statements TYPE i .

    METHODS determine_position
      IMPORTING
        !type         TYPE flag
        !index        TYPE i
      RETURNING
        VALUE(result) TYPE int4 .

    METHODS skip_keyword
      RETURNING
        VALUE(result) TYPE abap_bool .

    METHODS skip_statement
      IMPORTING
        !type         TYPE stmnt_type
      RETURNING
        VALUE(result) TYPE abap_bool .
ENDCLASS.



CLASS Y_CHECK_NUM_EXEC_STATEMENTS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Number of Executable Statements'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '600'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CI_NOES' ##NO_TEXT.
    settings-threshold = 40.
    settings-documentation = |{ c_docs_path-checks }number-executable-statements.md|.

    add_obj_type( c_type_program ).

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: &1 executable statements in method, exceeds threshold &2'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD count_statements.
    IF skip_statement( statement_wa-type ).
      RETURN.
    ENDIF.
    IF skip_keyword( ).
      RETURN.
    ENDIF.
    IF token_wa-type <> scan_token_type-comment AND token_wa-type <> scan_token_type-pragma.
      ADD 1 TO no_exec_statements.
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

    count_statements(
      CHANGING
        no_exec_statements = no_exec_statements ).

    IF index = structure-stmnt_to.
      DATA(check_configuration) = detect_check_configuration( threshold = no_exec_statements
                                                              include = get_include( p_level = statement_for_message-level ) ).
      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      IF no_exec_statements > check_configuration-threshold.
        raise_error( p_sub_obj_type = c_type_include
                     p_level        = statement_for_message-level
                     p_position     = determine_position( type = structure-type index = index )
                     p_from         = statement_for_message-from
                     p_kind         = check_configuration-prio
                     p_test         = me->myname
                     p_code         = get_code( check_configuration-prio )
                     p_param_1      = |{ no_exec_statements }|
                     p_param_2      = |{ check_configuration-threshold }| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD skip_keyword.
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
        OR if_kaizen_keywords_c=>gc_selection_screen
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


  METHOD skip_statement.
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
ENDCLASS.

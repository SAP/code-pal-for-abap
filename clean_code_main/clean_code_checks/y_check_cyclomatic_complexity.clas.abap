CLASS y_check_cyclomatic_complexity DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE seoclsname VALUE 'Y_CHECK_CYCLOMATIC_COMPLEXITY'.
    CONSTANTS second_token TYPE i VALUE 2.
    CONSTANTS third_token TYPE i VALUE 3.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS inspect_tokens
        REDEFINITION .
  PRIVATE SECTION.

    DATA cyclo_comp TYPE i .

    METHODS compute_cyclomatic_complexity
      CHANGING
        !c_cyclo_comp TYPE i .
    METHODS determine_position
      IMPORTING
        !type         TYPE flag
        !index        TYPE i
      RETURNING
        VALUE(result) TYPE int4 .
ENDCLASS.



CLASS y_check_cyclomatic_complexity IMPLEMENTATION.


  METHOD compute_cyclomatic_complexity.
    CASE keyword( ).
      WHEN 'IF' OR 'ELSEIF' OR 'WHILE' OR 'CHECK' OR
           'CATCH' OR 'CLEANUP' OR 'ASSERT' OR 'ENDAT' OR 'ENDSELECT' OR
           'LOOP' OR 'ON' OR 'PROVIDE'.
        ADD 1 TO c_cyclo_comp.
      WHEN 'WHEN'.
        IF get_token_rel( second_token ) <> 'OTHERS'.
          ADD 1 TO c_cyclo_comp.
        ENDIF.
      WHEN 'DO'.
        READ TABLE ref_scan_manager->get_tokens( ) INDEX statement_wa-to INTO DATA(token).
        IF token-str = 'TIMES'.
          ADD 1 TO c_cyclo_comp.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    description = 'Cyclomatic Complexity'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '150'.
    version     = '000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CI_CYCLO' ##NO_TEXT.
    settings-threshold = 10.
    settings-documentation = |{ c_docs_path-checks }cyclomatic-complexity.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Cyclomatic complexity is &1, exceeding threshold of &2'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
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
      cyclo_comp = 0.
    ENDIF.

    compute_cyclomatic_complexity(
      CHANGING
        c_cyclo_comp = cyclo_comp ).

    IF index = structure-stmnt_to.
      DATA(check_configuration) = detect_check_configuration( threshold = cyclo_comp
                                                              include = get_include( p_level = statement_for_message-level ) ).
      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      IF cyclo_comp > check_configuration-threshold.
        raise_error( p_sub_obj_type = c_type_include
                     p_level        = statement_for_message-level
                     p_position     = determine_position( type = structure-type index = index )
                     p_from         = statement_for_message-from
                     p_kind         = check_configuration-prio
                     p_test         = me->myname
                     p_code         = get_code( check_configuration-prio )
                     p_suppress     = settings-pseudo_comment
                     p_param_1      = |{ cyclo_comp }|
                     p_param_2      = |{ check_configuration-threshold }| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

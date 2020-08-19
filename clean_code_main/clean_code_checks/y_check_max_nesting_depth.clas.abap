CLASS y_check_max_nesting_depth DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS inspect_tokens
        REDEFINITION .
  PRIVATE SECTION.
    DATA statement_for_message TYPE sstmnt.
    DATA curr_nesting TYPE i .
    DATA max_nesting TYPE i .

    METHODS compute_nesting_level
      IMPORTING
        !token_str TYPE string .
    METHODS determine_position
      IMPORTING
        !type         TYPE flag
        !index        TYPE i
      RETURNING
        VALUE(result) TYPE int4 .
ENDCLASS.



CLASS Y_CHECK_MAX_NESTING_DEPTH IMPLEMENTATION.


  METHOD compute_nesting_level.
    CASE token_str.
      WHEN 'IF' OR 'WHILE'   OR 'LOOP' OR
           'DO' OR 'PROVIDE' OR 'TRY'  OR 'CASE'.
        ADD 1 TO curr_nesting.
      WHEN 'ENDIF' OR 'ENDWHILE'   OR 'ENDLOOP' OR
           'ENDDO' OR 'ENDPROVIDE' OR 'ENDTRY'  OR 'ENDCASE'.
        max_nesting = nmax( val1 = max_nesting val2 = curr_nesting ).
        SUBTRACT 1 FROM curr_nesting.
      WHEN 'ENDAT' OR 'ENDSELECT'.
        IF curr_nesting >= max_nesting.
          max_nesting = curr_nesting + 1.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    description = 'Nesting Depth'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '450'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CI_NESTING' ##NO_TEXT.
    settings-documentation = |{ c_docs_path-checks }maximum-nesting-depth.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Maximal nesting depth is &1, exceeding threshold of &2'(102)
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
    IF index = structure-stmnt_from.
      statement_for_message = statement.
      curr_nesting = 0.
      max_nesting = 0.
    ENDIF.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      compute_nesting_level( <token>-str ).

      IF index = structure-stmnt_to.
        DATA(check_configuration) = detect_check_configuration( error_count = max_nesting
                                                                include = get_include( p_level = statement_for_message-level ) ).
        IF check_configuration IS INITIAL.
          CONTINUE.
        ENDIF.

        IF max_nesting > check_configuration-threshold.
          raise_error( statement_level     = statement_for_message-level
                       statement_index     = determine_position( type = structure-type index = index )
                       statement_from      = statement_for_message-from
                       error_priority      = check_configuration-prio
                       parameter_01        = |{ max_nesting }|
                       parameter_02        = |{ check_configuration-threshold }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

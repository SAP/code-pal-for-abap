CLASS y_check_prefer_case_to_elseif DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF counter,
             if_structure TYPE sstruc,
             if_statement TYPE sstmnt,
             count        TYPE i,
           END OF counter.
    TYPES counters TYPE TABLE OF counter.

    METHODS skip IMPORTING structure TYPE sstruc
                 RETURNING VALUE(result) TYPE abap_bool.
    METHODS handle_result IMPORTING counters TYPE counters.
ENDCLASS.



CLASS y_check_prefer_case_to_elseif IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Prefer CASE to ELSE IF'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '0805'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC PREFER_CASE' ##NO_TEXT.
    settings-threshold = 5.
    settings-documentation = |{ c_docs_path-checks }prefer-case-to-elseif.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Prefer CASE to ELSE IF for multiple alternative conditions!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD execute_check.

    DATA counters TYPE counters.

    DATA(structures) = ref_scan_manager->get_structures( ).
    DATA(statements) = ref_scan_manager->get_statements( ).
    DATA(tokens) = ref_scan_manager->get_tokens( ).

    LOOP AT structures ASSIGNING FIELD-SYMBOL(<structure>)
    WHERE type EQ scan_struc_type-condition.

      IF skip( <structure> ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(structure) = structures[ <structure>-back ].
      DATA(statement) = statements[ structure-stmnt_from ].
      DATA(token) = tokens[ statement-from ].

      IF token-str <> 'IF'.
        CONTINUE.
      ENDIF.

      TRY.
          counters[ if_statement = statement ]-count = counters[ if_statement = statement ]-count + 1.
        CATCH cx_sy_itab_line_not_found.
          counters = VALUE #( BASE counters
                            ( if_structure = structure
                              if_statement = statement
                              count = 1 ) ).
      ENDTRY.

    ENDLOOP.

    handle_result( counters ).

  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD skip.
    CHECK test_code_detector->is_testcode( structure ) = abap_true.
    CHECK line_exists( check_configurations[ apply_on_testcode = abap_false ] ).
    result = abap_true.
  ENDMETHOD.


  METHOD handle_result.
    LOOP AT counters ASSIGNING FIELD-SYMBOL(<counter>).
      DATA(configuration) = detect_check_configuration( error_count = <counter>-count
                                                        statement = <counter>-if_statement ).

      IF configuration IS INITIAL.
        CONTINUE.
      ENDIF.

      raise_error( statement_level     = <counter>-if_statement-level
                   statement_index     = <counter>-if_structure-stmnt_from
                   statement_from      = <counter>-if_statement-from
                   error_priority      = configuration-prio ).
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.

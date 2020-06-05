CLASS y_check_constants_interface DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_CONSTANTS_INTERFACE' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    DATA has_something_else TYPE abap_bool VALUE abap_false.
    METHODS checkif_error
      IMPORTING index TYPE i.
ENDCLASS.



CLASS y_check_constants_interface IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                            include = get_include( p_level = statement_for_message-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF has_something_else EQ abap_false.
      raise_error( p_sub_obj_type = c_type_include
                   p_level        = statement_for_message-level
                   p_position     = index
                   p_from         = statement_for_message-from
                   p_kind         = check_configuration-prio
                   p_test         = me->myname
                   p_code         = get_code( check_configuration-prio )
                   p_suppress     = settings-pseudo_comment ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    description = 'Constants Interface'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '125'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC CONS_INTF' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }constants-interface.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: There are only constants in this interface!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
          WHERE stmnt_type EQ scan_struc_stmnt_type-interface.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      READ TABLE ref_scan_manager->get_statements( ) INTO statement_for_message
        INDEX <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to WHERE type NE scan_stmnt_type-comment AND
                                                                  type NE scan_stmnt_type-comment_in_stmnt AND
                                                                  type NE scan_stmnt_type-pragma.
        inspect_tokens( statement = <statement> ).
      ENDLOOP.

      checkif_error( <structure>-stmnt_from ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(token) = get_token_abs( statement-from ).
    IF token NE 'CONSTANTS' AND
       token NE 'INTERFACE' AND
       token NE 'ENDINTERFACE' AND
       token NE 'BEGIN' AND
       token NE 'END' AND
       token NE 'OF'.
      has_something_else = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

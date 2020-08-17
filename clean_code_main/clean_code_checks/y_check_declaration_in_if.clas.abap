CLASS y_check_declaration_in_if DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA branch_counter TYPE i VALUE 0 ##NO_TEXT.
    CONSTANTS first_if TYPE i VALUE 1 ##NO_TEXT.

    METHODS check_if_error
      IMPORTING index     TYPE i
                keyword   TYPE string
                statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_DECLARATION_IN_IF IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Declaration in IF'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position    = '210'.
    version     = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC DECL_IN_IF' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-prio = 'W'.
    settings-documentation = |{ c_docs_path-checks }declaration-in-if.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Declarations in IF-Blocks should be removed!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-if.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      DATA(index) = <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( index     = index
                        statement = <statement> ).

        index = index + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(keyword) = get_token_abs( statement-from ).

    CASE keyword.
      WHEN 'IF'.
        branch_counter = branch_counter + 1.

      WHEN 'ENDIF'.
        branch_counter = branch_counter - 1.

      WHEN 'DATA' OR 'FIELD-SYMBOLS' OR 'TYPES'.
        check_if_error( index = index
                        keyword = keyword
                        statement = statement ).
    ENDCASE.

    IF keyword CP 'DATA(*)'.
      check_if_error( index = index
                      keyword = keyword
                      statement = statement ).
    ENDIF.
  ENDMETHOD.


  METHOD check_if_error.
    IF branch_counter = first_if.

      DATA(check_configuration) = detect_check_configuration( threshold = 0
                                                              include = get_include( p_level = statement-level ) ).
      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio
                   parameter_01        = |{ keyword }| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

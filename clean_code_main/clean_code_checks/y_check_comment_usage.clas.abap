CLASS y_check_comment_usage DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_myname TYPE sci_chk VALUE 'Y_CHECK_COMMENT_USAGE' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.

    DATA abs_statement_number TYPE i VALUE 0.
    DATA comment_number TYPE i VALUE 0.
    DATA percentage_of_comments TYPE decfloat16 VALUE 0.
    DATA is_function_module TYPE abap_bool.

    METHODS calc_percentage_of_comments .
    METHODS checkif_error
      IMPORTING
        !index TYPE i .

    METHODS is_code_disabled
      IMPORTING structure     TYPE sstruc
                statement     TYPE sstmnt
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_CHECK_COMMENT_USAGE IMPLEMENTATION.


  METHOD calc_percentage_of_comments.
    percentage_of_comments = ( comment_number / abs_statement_number ) * 100.
    percentage_of_comments = round( val = percentage_of_comments dec = 2 ).
  ENDMETHOD.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( threshold = round( val =  percentage_of_comments
                                                                               dec = 0
                                                                               mode = cl_abap_math=>round_down )
                                                            include = get_include( p_level = statement_for_message-level ) ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    IF percentage_of_comments GT check_configuration-threshold.
      raise_error( p_sub_obj_type = c_type_include
                   p_level        = statement_for_message-level
                   p_position     = index
                   p_from         = statement_for_message-from
                   p_kind         = check_configuration-prio
                   p_test         = me->myname
                   p_code         = get_code( check_configuration-prio )
                   p_param_1      = |{ comment_number }|
                   p_param_2      = |{ percentage_of_comments }|
                   p_param_3      = |{ check_configuration-threshold }| ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    description = 'Comment Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    position = '120'.
    version = '0000'.
    has_documentation = abap_true.

    settings-prio = 'N'.
    settings-threshold = 10.
    settings-documentation = |{ c_docs_path-checks }comment-usage.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: &1 comments found! This is &2% of the productive code, exceeding threshold of &3%'(102)
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
       WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition
          OR stmnt_type EQ scan_struc_stmnt_type-class_implementation
          OR stmnt_type EQ scan_struc_stmnt_type-interface
          OR stmnt_type EQ scan_struc_stmnt_type-form
          OR stmnt_type EQ scan_struc_stmnt_type-function
          OR stmnt_type EQ scan_struc_stmnt_type-module
          OR type EQ scan_struc_type-event.

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

      abs_statement_number = 0.
      comment_number = 0.
      percentage_of_comments = 0.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.
        inspect_tokens( statement = <statement>
                        structure = <structure> ).
      ENDLOOP.

      calc_percentage_of_comments( ).
      checkif_error( <structure>-stmnt_from ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK is_code_disabled( statement = statement
                            structure = structure ) EQ abap_false.

    IF statement-to EQ statement-from.
      abs_statement_number = abs_statement_number + 1.
    ELSE.
      abs_statement_number = abs_statement_number + ( statement-to - statement-from ).
    ENDIF.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to
      WHERE type EQ scan_token_type-comment.

      IF strlen( <token>-str ) GE 2 AND NOT
         ( <token>-str+0(2) EQ |*"| OR
           <token>-str+0(2) EQ |"!| OR
           <token>-str+0(2) EQ |##| OR
           <token>-str+0(2) EQ |*?| OR
           ( strlen( <token>-str ) GE 3 AND <token>-str+0(3) EQ |"#E| ) OR
           <token>-str CP '"' && object_name && '*.' ).
        comment_number = comment_number + 1.
      ENDIF.
    ENDLOOP.
    UNASSIGN <token>.
  ENDMETHOD.


  METHOD is_code_disabled.
    CHECK structure-stmnt_type EQ scan_struc_stmnt_type-function.

    IF get_token_abs( statement-from ) EQ if_kaizen_keywords_c=>gc_function.
      is_function_module = abap_true.
    ELSEIF get_token_abs( statement-from ) EQ if_kaizen_keywords_c=>gc_endfunction.
      is_function_module = abap_false.
    ENDIF.

    result = xsdbool( is_function_module EQ abap_false ).
  ENDMETHOD.
ENDCLASS.

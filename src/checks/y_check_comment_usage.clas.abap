CLASS y_check_comment_usage DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.

    DATA abs_statement_number TYPE i VALUE 0.
    DATA comment_number TYPE i VALUE 0.
    DATA percentage_of_comments TYPE decfloat16 VALUE 0.
    DATA is_function_module TYPE abap_bool.
    DATA statement_for_message TYPE sstmnt.

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

    DATA(check_configuration) = detect_check_configuration( error_count = round( val =  percentage_of_comments
                                                                                 dec = 0
                                                                                 mode = cl_abap_math=>round_down )
                                                            statement = statement_for_message ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement_for_message-level
                 statement_index     = index
                 statement_from      = statement_for_message-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ comment_number }|
                 parameter_02        = |{ percentage_of_comments }|
                 parameter_03        = |{ check_configuration-threshold }| ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-prio = c_note.
    settings-threshold = 10.
    settings-documentation = |{ c_docs_path-checks }comment-usage.md|.

    set_check_message( '&1 comments found! This is &2% of the productive code reaching threshold of &3%!' ).
  ENDMETHOD.


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

      abs_statement_number = 0.
      comment_number = 0.
      percentage_of_comments = 0.

      READ TABLE ref_scan_manager->get_statements( ) INTO statement_for_message
        INDEX <structure>-stmnt_from.

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
           <token>-str+0(2) EQ |"?| OR
           ( strlen( <token>-str ) GE 3 AND <token>-str+0(3) EQ |"#E| ) OR
           <token>-str CP '"' && object_name && '*.' ). "#EC CI_MAGIC
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

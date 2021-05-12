CLASS y_check_comment_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA abs_statement_number TYPE i VALUE 0.
    DATA comment_number TYPE i VALUE 0.
    DATA is_function_module TYPE abap_bool.

    METHODS get_percentage_of_comments RETURNING VALUE(result) TYPE int4.

    METHODS check_result IMPORTING structure TYPE sstruc.

    METHODS is_code_disabled IMPORTING structure TYPE sstruc
                                       statement TYPE sstmnt
                             RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_check_comment_usage IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-threshold = 10.
    settings-documentation = |{ c_docs_path-checks }comment-usage.md|.
    settings-ignore_pseudo_comments = abap_true.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-class_implementation )
                                        ( scan_struc_stmnt_type-interface )
                                        ( scan_struc_stmnt_type-form )
                                        ( scan_struc_stmnt_type-function )
                                        ( scan_struc_stmnt_type-module ) ).

    set_check_message( 'Percentage of comments must be lower than &3% of the productive code! (&2%>=&3%) (&1 lines found)' ).
  ENDMETHOD.


  METHOD inspect_statements.
    abs_statement_number = 0.
    comment_number = 0.

    super->inspect_statements( structure ).

    check_result( structure ).
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(code_disabled) = is_code_disabled( statement = statement
                                            structure = structure ).
    IF code_disabled = abap_true.
      RETURN.
    ENDIF.

    IF statement-to = statement-from.
      abs_statement_number = abs_statement_number + 1.
    ELSE.
      abs_statement_number = abs_statement_number + ( statement-to - statement-from ).
    ENDIF.

    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE type = scan_token_type-comment.
      IF strlen( <token>-str ) >= 2 AND NOT
         ( <token>-str+0(2) = |*"| OR
           <token>-str+0(2) = |"!| OR
           <token>-str+0(2) = |##| OR
           <token>-str+0(2) = |*?| OR
           <token>-str+0(2) = |"?| OR
           ( strlen( <token>-str ) >= 3 AND <token>-str+0(3) = |"#E| ) OR
           <token>-str CP '"' && object_name && '*.' ).   "#EC CI_MAGIC
        comment_number = comment_number + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_result.
    DATA(percentage_of_comments) = get_percentage_of_comments( ).

    DATA(statement_for_message) = ref_scan_manager->statements[ structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( error_count = percentage_of_comments
                                                            statement = statement_for_message ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement_for_message-level
                 statement_index = structure-stmnt_from
                 statement_from = statement_for_message-from
                 error_priority = check_configuration-prio
                 parameter_01 = |{ comment_number }|
                 parameter_02 = |{ percentage_of_comments }|
                 parameter_03 = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD get_percentage_of_comments.
    DATA percentage TYPE decfloat16.

    percentage = ( comment_number / abs_statement_number ) * 100.

    result = round( val = percentage
                    dec = 0
                    mode = cl_abap_math=>round_down ).
  ENDMETHOD.


  METHOD is_code_disabled.
    CHECK structure-stmnt_type = scan_struc_stmnt_type-function.

    IF get_token_abs( statement-from ) = if_kaizen_keywords_c=>gc_function.
      is_function_module = abap_true.
    ELSEIF get_token_abs( statement-from ) = if_kaizen_keywords_c=>gc_endfunction.
      is_function_module = abap_false.
    ENDIF.

    result = xsdbool( is_function_module = abap_false ).
  ENDMETHOD.
ENDCLASS.

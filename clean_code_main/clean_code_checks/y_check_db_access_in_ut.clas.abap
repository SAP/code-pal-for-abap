CLASS y_check_db_access_in_ut DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.

    METHODS is_persistent_object
      IMPORTING
        !obj_name     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool .

    METHODS check_if_error
      IMPORTING
        !index     TYPE i OPTIONAL
        !statement TYPE sstmnt OPTIONAL .
ENDCLASS.



CLASS Y_CHECK_DB_ACCESS_IN_UT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Database Access within Unit Tests'(001).
    category  = 'Y_CHECK_CATEGORY'.
    position  = '180'.
    version   = '0000'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC DB_ACCESS_UT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_productive_code = abap_false.
    settings-apply_on_test_code = abap_true.
    settings-documentation = |{ c_docs_path-checks }db-access-in-ut.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Database access(es) within a Unit-Test should be removed!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type EQ scan_struc_stmnt_type-method.

      is_testcode = test_code_detector->is_testcode( <structure> ).
      IF is_testcode EQ abap_false.
        CONTINUE.
      ENDIF.

      DATA(index) = <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( index = index
                        statement = <statement> ).

        index = index + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    DATA(token1) = get_token_abs( statement-from ).
    DATA(token2) = get_token_abs( statement-from + 1 ).
    DATA(token3) = get_token_abs( statement-from + 2 ).

    DATA(has_db_keyword) = xsdbool( token1 = 'COMMIT'
                                 OR token1 = 'ROLLBACK'
                                 OR token1 = 'SELECT'
                                 OR token1 = 'EXEC'
                                 OR token1 = 'ALTER' ).

    DATA(is_name_in_token3) = xsdbool( ( token1 = 'INSERT' AND token2 = 'INTO' )
                                    OR ( token1 = 'DELETE' AND token2 = 'FROM' ) ).

    DATA(is_name_in_token2) = xsdbool( ( token1 = 'INSERT'
                                      OR token1 = 'UPDATE'
                                      OR token1 = 'MODIFY'
                                      OR token1 = 'DELETE' ) AND token3 = 'FROM' ).

    IF has_db_keyword = abap_true
     OR ( is_name_in_token3 = abap_true AND is_persistent_object( to_upper( token3 ) ) = abap_true )
     OR ( is_name_in_token2 = abap_true AND is_persistent_object( to_upper( token2 ) ) = abap_true ).

      check_if_error( index = index
                      statement = statement ).
    ENDIF.
  ENDMETHOD.


  METHOD is_persistent_object.

    TRY.
        SELECT SINGLE devclass FROM tadir INTO @DATA(package)
          WHERE pgmid = 'R3TR' AND
                object = 'TABL' AND
                obj_name = @obj_name.
        IF sy-subrc NE 0.
          result = abap_false.
          RETURN.
        ENDIF.

        DATA(checked_object) = cl_abap_dyn_prg=>check_table_name_str(
             val             = obj_name
             packages        = package ).

        DATA dynamic_line TYPE REF TO data.
        FIELD-SYMBOLS <table_structure> TYPE any.

        CREATE DATA dynamic_line TYPE (checked_object).
        ASSIGN dynamic_line->* TO <table_structure>.

        SELECT SINGLE * FROM (checked_object) INTO <table_structure>.
        result = abap_true.

      CATCH cx_abap_not_a_table cx_abap_not_in_package cx_sy_dynamic_osql_semantics cx_root.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD check_if_error.
    DATA check_configuration TYPE y_if_clean_code_manager=>check_configuration.
    DATA(key_word) = get_token_abs( statement-from ).

    check_configuration = detect_check_configuration( error_count = 0
                                                      include = get_include( p_level = statement-level ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ key_word }| ).

  ENDMETHOD.
ENDCLASS.

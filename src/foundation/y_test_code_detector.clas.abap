CLASS y_test_code_detector DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      y_if_testcode_detector.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      ref_scan_manager TYPE REF TO y_if_scan_manager,
      statement_wa     TYPE sstmnt,
      test_codes       TYPE y_if_testcode_detector=>t_test_codes,
      test_code        TYPE y_if_testcode_detector=>t_test_code.
    METHODS:
      process_statements
        IMPORTING structure TYPE sstruc,
      process_tokens
        IMPORTING statement TYPE sstmnt,
      try_testclass
        IMPORTING token         TYPE stokesx
        RETURNING VALUE(result) TYPE abap_bool,
      try_testmethod
        RETURNING VALUE(result) TYPE abap_bool,
      keyword
        RETURNING VALUE(result) TYPE string,
      get_token_rel
        IMPORTING p_n           TYPE i
        RETURNING VALUE(result) TYPE string,
      determine_test_code,
      is_test_class
        RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_TEST_CODE_DETECTOR IMPLEMENTATION.


  METHOD determine_test_code.
    IF test_codes IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ref_scan_manager->structures ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type = scan_struc_stmnt_type-class_definition.

      process_statements( <structure> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_token_rel.
    DATA(l_index) = statement_wa-from + p_n - 1.
    IF l_index > statement_wa-to.
      RETURN.
    ENDIF.
    READ TABLE ref_scan_manager->tokens INDEX l_index INTO DATA(token_wa).
    result = token_wa-str.
  ENDMETHOD.


  METHOD is_test_class.
    IF keyword( ) = 'CLASS'.
      DATA(class) = get_token_rel( 2 ). "#EC DECL_IN_IF
      result = xsdbool( line_exists( test_codes[ class = class ] ) ).
    ENDIF.
  ENDMETHOD.


  METHOD keyword.
    IF statement_wa-type = 'C'.
      result = 'COMPUTE'.
      RETURN.
    ENDIF.
    READ TABLE ref_scan_manager->tokens INDEX statement_wa-from INTO DATA(token_wa).
    result = token_wa-str.
  ENDMETHOD.


  METHOD process_statements.
    CLEAR test_code.

    LOOP AT ref_scan_manager->statements ASSIGNING FIELD-SYMBOL(<statement>)
      FROM structure-stmnt_from TO structure-stmnt_to.

      statement_wa = <statement>.
      process_tokens( <statement> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD process_tokens.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      IF try_testclass( <token> ).
        EXIT.
      ENDIF.

      IF try_testmethod( ).
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD try_testclass.
    IF token-str = 'TESTING' AND
       keyword( ) = 'CLASS'.
      test_code-class = get_token_rel( 2 ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD try_testmethod.
    IF test_code-class IS NOT INITIAL
    AND ( keyword( ) = 'METHODS' OR keyword( ) = 'CLASS-METHODS' ).
      test_code-method = get_token_rel( 2 ).
      APPEND test_code TO test_codes.
      result = abap_true.
    ENDIF.

    IF test_code-class IS NOT INITIAL
    AND test_code-method IS INITIAL
    AND keyword( ) = 'ENDCLASS'.
      APPEND test_code TO test_codes.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_testcode_detector~clear.
    CLEAR ref_scan_manager.
    CLEAR test_codes.
    CLEAR test_code.
    CLEAR statement_wa.
  ENDMETHOD.


  METHOD y_if_testcode_detector~is_testcode.
    determine_test_code( ).
    IF test_codes IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE ref_scan_manager->statements INTO statement_wa INDEX structure-stmnt_from.
    IF is_test_class( ).
      result = abap_true.

    ELSE.
      DATA(high_level_structure) = structure. "#EC DECL_IN_IF

      DO.
        DATA(low_level_structure) = high_level_structure. "#EC DECL_IN_IF
        READ TABLE ref_scan_manager->structures INTO high_level_structure INDEX low_level_structure-back.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        READ TABLE ref_scan_manager->statements INTO statement_wa INDEX high_level_structure-stmnt_from.
        IF is_test_class( ).
          result = abap_true.
          EXIT.
        ENDIF.
        IF low_level_structure-back = 0.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_testcode_detector~set_ref_scan_manager.
    me->ref_scan_manager = ref_scan_manager. "#EC SELF_REF
  ENDMETHOD.
ENDCLASS.

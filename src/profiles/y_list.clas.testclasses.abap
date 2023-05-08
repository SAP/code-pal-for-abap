CLASS lcl_list_ut DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO y_if_list.

    METHODS setup.

    METHODS integration_tests FOR TESTING.

    METHODS get_char_at
      IMPORTING index         TYPE i
      RETURNING VALUE(result) TYPE char1.

    METHODS:
      get_type_name,
      append,
      insert_at,
      modify_at,
      delete_at,
      delete_all,
      append_after_delete,
      is_contained_false,
      is_contained_true,
      set_table,
      get_table,
      get_line_index.

ENDCLASS.

CLASS lcl_list_ut IMPLEMENTATION.
  METHOD setup.
    TRY.
        cut = NEW y_list( '01928374655647382910' ).
        cl_abap_unit_assert=>fail( msg   = 'Unit Test Aborted: - Type Creation Error!'
                                   level = if_aunit_constants=>severity-high
                                   quit  = if_aunit_constants=>quit-test ).
      CATCH cx_sy_create_data_error.

        TRY.
            cut = NEW y_list( 'c' ).
          CATCH cx_sy_create_data_error.
            cl_abap_unit_assert=>fail( msg   = 'Unit Test Aborted: - Type Creation Error!'
                                       level = if_aunit_constants=>severity-high
                                       quit  = if_aunit_constants=>quit-test ).
        ENDTRY.

    ENDTRY.
  ENDMETHOD.

  METHOD get_char_at.
    FIELD-SYMBOLS: <line> TYPE any.
    DATA(dta) = cut->get_line_at( index ).
    ASSIGN dta->* TO <line>.
    IF sy-subrc = 0.
      result = <line>.
    ENDIF.
  ENDMETHOD.

  METHOD integration_tests.
    get_type_name( ).
    append( ).
    insert_at( ).
    get_line_index( ).
    modify_at( ).
    delete_at( ).
    delete_all( ).
    append_after_delete( ).
    is_contained_false( ).
    is_contained_true( ).
    set_table( ).
    get_table( ).
  ENDMETHOD.

  METHOD get_type_name.
    cl_abap_unit_assert=>assert_equals( exp = 'c'
                                        act = cut->get_type_name( )
                                        msg = 'typename error!'
                                        quit = if_aunit_constants=>quit-test ).
  ENDMETHOD.

  METHOD append.
    cut->append( 'Bbb' ).
    cut->append( 'c' ).
    cut->append( 'A' ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = cut->number_of_rows( )
                                        msg = 'component are not append!'
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( exp = 'A'
                                        act = get_char_at( 1 )
                                        msg = 'component are not append!'
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( exp = 'B'
                                        act = get_char_at( 2 )
                                        msg = 'component are not append!'
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( exp = 'c'
                                        act = get_char_at( 3 )
                                        msg = 'component are not append!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD insert_at.
    cut->insert_at( line  = 'd'
                    index = 1 ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = cut->number_of_rows( )
                                        msg = 'component are not insert!'
                                        quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( exp = 'd'
                                        act = get_char_at( 4 )
                                        msg = 'component are not insert!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD modify_at.
    cut->modify_at( line   = 'E'
                    index = 1 ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = cut->number_of_rows( )
                                        msg = 'component are not modified!'
                                        quit  = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = cut->is_contained( 'E' )
                                        msg = 'component are not modified!'
                                        quit  = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD delete_at.
    cut->delete_at( 4 ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = cut->number_of_rows( )
                                        msg = 'component are not deleted!'
                                        quit  = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = cut->is_contained( 'd' )
                                        msg = 'component are not deleted!'
                                        quit  = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD delete_all.
    cut->delete_all( ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = cut->number_of_rows( )
                                        msg = 'table is not deleted!'
                                        quit  = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD append_after_delete.
    cut->append( 'abc' ).

    cl_abap_unit_assert=>assert_equals( exp = 'a'
                                        act = get_char_at( 1 )
                                        msg = 'component are not append!'
                                        quit  = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_differs( exp = 'abc'
                                         act = get_char_at( 1 )
                                         msg = 'component are not one char!'
                                         quit  = if_aunit_constants=>quit-no ).

    cut->append( 123 ).
  ENDMETHOD.

  METHOD is_contained_false.
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = cut->is_contained( 'b' )
                                        msg = 'component are exist!'
                                        quit  = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD is_contained_true.
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = cut->is_contained( 'a' )
                                        msg = 'component are not exist!'
                                        quit  = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD set_table.
    DATA table TYPE STANDARD TABLE OF char1.
    APPEND 'a' TO table.
    APPEND 'b' TO table.
    APPEND 'c' TO table.
    APPEND 'd' TO table.
    APPEND 'e' TO table.

    cut->set_table( table ).

    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = cut->number_of_rows( )
                                        msg = 'cannot set the table!'
                                        quit  = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD get_table.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    DATA(dta) = cut->get_table( ).

    ASSIGN dta->* TO <table>.

    cl_abap_unit_assert=>assert_equals( exp = lines( <table> )
                                        act = cut->number_of_rows( )
                                        msg = 'cannot get the table!'
                                        quit  = if_aunit_constants=>quit-no ).
    UNASSIGN <table>.
  ENDMETHOD.

  METHOD get_line_index.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = cut->get_line_index( 'c' )
                                        msg = 'cannot get line index!'
                                        quit  = if_aunit_constants=>quit-no ).
  ENDMETHOD.

ENDCLASS.

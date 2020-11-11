*"* use this source file for your ABAP unit test classes
CLASS local_test_class DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS validate_positions FOR TESTING.
  PRIVATE SECTION.
    CONSTANTS first_object TYPE sci_chk VALUE 'Y_CHECK_A'.
    CONSTANTS second_object TYPE sci_chk VALUE 'Y_CHECK_B'.
    CONSTANTS tirth_object TYPE sci_chk VALUE 'Y_CHECK_C'.
    CONSTANTS fourth_object TYPE sci_chk VALUE 'Y_CHECK_D'.

    CLASS-DATA environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'TADIR' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Standard table will not sort the entries
    DATA tadir TYPE STANDARD TABLE OF tadir.

    " Non-sorted entries are mandatory to validate the test
    tadir = VALUE #( ( pgmid = 'R3T3' object = 'CLAS' obj_name = tirth_object  devclass = '$CODE_PAL_FOR_ABAP_CHECKS' )
                     ( pgmid = 'R3T3' object = 'CLAS' obj_name = fourth_object devclass = '$CODE_PAL_FOR_ABAP_CHECKS' )
                     ( pgmid = 'R3T3' object = 'CLAS' obj_name = first_object  devclass = '$CODE_PAL_FOR_ABAP_CHECKS' )
                     ( pgmid = 'R3T3' object = 'CLAS' obj_name = second_object devclass = '$CODE_PAL_FOR_ABAP_CHECKS' ) ).

    " Required to find the checks
    tadir = VALUE #( BASE tadir
                     ( pgmid = 'R3T3' object = 'CLAS' obj_name = 'Y_CHECK_BASE' devclass = '$CODE_PAL_FOR_ABAP_FOUNDATION' ) ).

    environment->insert_test_data( tadir ).
  ENDMETHOD.

  METHOD teardown.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD validate_positions.
    cl_abap_unit_assert=>assert_equals( act = y_code_pal_sorter=>get_position( first_object )
                                        exp = '001' ).

    cl_abap_unit_assert=>assert_equals( act = y_code_pal_sorter=>get_position( second_object )
                                        exp = '002' ).

    cl_abap_unit_assert=>assert_equals( act = y_code_pal_sorter=>get_position( tirth_object )
                                        exp = '003' ).

    cl_abap_unit_assert=>assert_equals( act = y_code_pal_sorter=>get_position( fourth_object )
                                        exp = '004' ).
  ENDMETHOD.

ENDCLASS.

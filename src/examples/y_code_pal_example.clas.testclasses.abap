*"* use this source file for your ABAP unit test classes
CLASS local_test_class DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS db_access_in_ut FOR TESTING.
    METHODS external_call_in_ut FOR TESTING.
    METHODS unit_test_assert FOR TESTING.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD db_access_in_ut.
    SELECT SINGLE * FROM t100
    INTO @DATA(entry).

    cl_abap_unit_assert=>assert_not_initial( entry ).
  ENDMETHOD.

  METHOD external_call_in_ut.
    DATA alv TYPE REF TO cl_gui_alv_grid.
    cl_abap_unit_assert=>skip( 'No Gui Allowed' ).
  ENDMETHOD.

  METHOD unit_test_assert.
    "Given
    DATA(first) = 10.
    DATA(second) = 20.
    "When
    DATA(sum) = first + second.
    "Then
    cl_abap_unit_assert=>assert_equals( act = sum
                                        exp = sum ).
  ENDMETHOD.

ENDCLASS.

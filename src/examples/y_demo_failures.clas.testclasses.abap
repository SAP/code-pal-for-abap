*"* use this source file for your ABAP unit test classes
CLASS local_test_class DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS db_access_in_ut FOR TESTING.
    METHODS external_call_in_ut FOR TESTING.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD db_access_in_ut.
    SELECT SINGLE * FROM t100
    INTO @DATA(entry).
  ENDMETHOD.

  METHOD external_call_in_ut.
    DATA alv TYPE REF TO cl_gui_alv_grid.
  ENDMETHOD.

ENDCLASS.

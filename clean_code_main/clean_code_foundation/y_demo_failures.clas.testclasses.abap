*"* use this source file for your ABAP unit test classes
CLASS ltc_error_fail DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO y_demo_failures.

    METHODS:
      setup,
      db_access_in_ut      FOR TESTING,
      external_call_in_ut  FOR TESTING.
ENDCLASS.

CLASS ltc_error_fail IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_demo_failures( ).
  ENDMETHOD.

  METHOD db_access_in_ut.
    cut->attribute_1 = 'DSAG'.
    SELECT * FROM t100 UP TO 1 ROWS INTO @DATA(result). "#EC DB_ACCESS_UT
    ENDSELECT.
    cl_abap_unit_assert=>assert_equals(
      act = 1
      exp = 1 ).
  ENDMETHOD.

  METHOD external_call_in_ut.

    SUBMIT demo_program_submit_rep AND RETURN.  "#EC EXT_CALL_UT

    DATA date TYPE sy-datum.
    CALL FUNCTION 'DATE_TO_DAY' DESTINATION ''
      EXPORTING
        date = sy-datum
      IMPORTING
         weekday = date.

    CALL FUNCTION 'DATE_TO_DAY' IN UPDATE TASK
      EXPORTING
        date = sy-datum
      IMPORTING
         weekday = date.

    CALL FUNCTION 'DATE_TO_DAY' STARTING NEW TASK 'task' DESTINATION ''
      EXPORTING
        date = sy-datum.

  ENDMETHOD.

ENDCLASS.

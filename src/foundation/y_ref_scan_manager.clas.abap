CLASS y_ref_scan_manager DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_scan_manager.

  PRIVATE SECTION.
    DATA ref_scan TYPE REF TO cl_ci_scan.

ENDCLASS.

CLASS y_ref_scan_manager IMPLEMENTATION.


  METHOD y_if_scan_manager~is_scan_ok.
    CONSTANTS ok TYPE syst_subrc VALUE 0.
    CHECK ref_scan IS NOT INITIAL.
    result = xsdbool( ref_scan->subrc = ok ).
  ENDMETHOD.


  METHOD y_if_scan_manager~set_ref_scan.
    ref_scan = io_ref_scan.

    y_if_scan_manager~levels = COND #( WHEN ref_scan IS NOT INITIAL THEN ref_scan->levels ).
    y_if_scan_manager~structures = COND #( WHEN ref_scan IS NOT INITIAL THEN ref_scan->structures ).
    y_if_scan_manager~statements = COND #( WHEN ref_scan IS NOT INITIAL THEN ref_scan->statements ).
    y_if_scan_manager~tokens = COND #( WHEN ref_scan IS NOT INITIAL THEN ref_scan->tokens ).
  ENDMETHOD.


ENDCLASS.

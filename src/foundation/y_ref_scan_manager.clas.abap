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

    IF ref_scan IS NOT BOUND.
      RETURN.
    ENDIF.

    y_if_scan_manager~levels = ref_scan->levels.
    y_if_scan_manager~structures = ref_scan->structures.
    y_if_scan_manager~statements = ref_scan->statements.
    y_if_scan_manager~tokens = ref_scan->tokens.
  ENDMETHOD.


ENDCLASS.

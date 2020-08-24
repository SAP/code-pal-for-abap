CLASS y_ref_scan_manager DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_manager.
    CONSTANTS: ok TYPE syst_subrc VALUE 0.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ref_scan TYPE REF TO cl_ci_scan.
ENDCLASS.



CLASS Y_REF_SCAN_MANAGER IMPLEMENTATION.


  METHOD y_if_scan_manager~get_levels.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->levels.
  ENDMETHOD.


  METHOD y_if_scan_manager~get_statements.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->statements.
  ENDMETHOD.


  METHOD y_if_scan_manager~get_structures.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->structures.
  ENDMETHOD.


  METHOD y_if_scan_manager~get_tokens.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->tokens.
  ENDMETHOD.


  METHOD y_if_scan_manager~is_scan_ok.
    CHECK ref_scan IS NOT INITIAL.

    result = abap_true.
    IF ref_scan->subrc <> ok.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_scan_manager~set_ref_scan.
    ref_scan = io_ref_scan.
  ENDMETHOD.
ENDCLASS.

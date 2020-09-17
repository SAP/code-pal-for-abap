CLASS y_ref_scan_manager DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_scan_manager.
    CONSTANTS ok TYPE syst_subrc VALUE 0.
    ALIASES get_levels FOR y_if_scan_manager~get_levels.
    ALIASES get_statements FOR y_if_scan_manager~get_statements.
    ALIASES get_structures FOR y_if_scan_manager~get_structures.
    ALIASES get_tokens FOR y_if_scan_manager~get_tokens.
    ALIASES is_scan_ok FOR y_if_scan_manager~is_scan_ok.
    ALIASES set_ref_scan FOR y_if_scan_manager~set_ref_scan.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ref_scan TYPE REF TO cl_ci_scan.
ENDCLASS.

CLASS y_ref_scan_manager IMPLEMENTATION.

  METHOD get_levels.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->levels.
  ENDMETHOD.

  METHOD get_statements.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->statements.
  ENDMETHOD.

  METHOD get_structures.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->structures.
  ENDMETHOD.

  METHOD get_tokens.
    CHECK ref_scan IS NOT INITIAL.
    result = ref_scan->tokens.
  ENDMETHOD.

  METHOD is_scan_ok.
    CHECK ref_scan IS NOT INITIAL.

    result = abap_true.
    IF ref_scan->subrc <> ok.
      result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD set_ref_scan.
    ref_scan = io_ref_scan.
  ENDMETHOD.

ENDCLASS.

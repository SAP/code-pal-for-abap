CLASS y_ref_scan_manager DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_scan_manager.

  PRIVATE SECTION.
    DATA ref_scan TYPE REF TO cl_ci_scan.
    DATA in_scope_levels LIKE y_if_scan_manager~levels.

    METHODS define_in_scope_levels.

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

    define_in_scope_levels( ).
  ENDMETHOD.


  METHOD define_in_scope_levels.
    TRY.
        DATA(main_level) = y_if_scan_manager~levels[ level = 0 ].
        DATA(main_application_component) = y_code_pal_app_comp=>get( main_level ).
      CATCH cx_sy_itab_line_not_found
            ycx_entry_not_found.
        RETURN.
    ENDTRY.

    LOOP AT y_if_scan_manager~levels ASSIGNING FIELD-SYMBOL(<level>).
      TRY.
          DATA(application_component) = y_code_pal_app_comp=>get( <level> ).
          IF main_application_component = application_component.
             APPEND <level> TO in_scope_levels.
          ENDIF.
        CATCH ycx_entry_not_found.
          APPEND <level> TO in_scope_levels.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD y_if_scan_manager~is_level_in_scope.
    result = xsdbool( line_exists( in_scope_levels[ table_line = level ] ) ).
  ENDMETHOD.


ENDCLASS.

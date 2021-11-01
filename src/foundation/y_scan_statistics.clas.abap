CLASS y_scan_statistics DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_statistics.

ENDCLASS.



CLASS Y_SCAN_STATISTICS IMPLEMENTATION.

  METHOD y_if_scan_statistics~collect.
    IF pc = cl_ci_test_root=>c_pc_exceptn_exists.
      y_if_scan_statistics~count-pseudo_comments = y_if_scan_statistics~count-pseudo_comments + 1.
    ELSEIF kind = y_check_base=>c_error.
      y_if_scan_statistics~count-errors = y_if_scan_statistics~count-errors + 1.
    ELSEIF kind = y_check_base=>c_warning.
      y_if_scan_statistics~count-warnings = y_if_scan_statistics~count-warnings + 1.
    ELSEIF kind = y_check_base=>c_note.
      y_if_scan_statistics~count-notes = y_if_scan_statistics~count-notes + 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

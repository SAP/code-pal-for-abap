CLASS y_scan_statistics DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_statistics.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: number_errors          TYPE i,
          number_warnings        TYPE i,
          number_notes           TYPE i,
          number_pseudo_comments TYPE i.
ENDCLASS.



CLASS Y_SCAN_STATISTICS IMPLEMENTATION.


  METHOD y_if_scan_statistics~collect.
    IF pc = cl_ci_test_root=>c_pc_exceptn_exists.
      number_pseudo_comments = number_pseudo_comments + 1.
    ELSEIF kind = y_check_base=>c_error.
      number_errors = number_errors + 1.
    ELSEIF kind = y_check_base=>c_warning.
      number_warnings = number_warnings + 1.
    ELSEIF kind = y_check_base=>c_note.
      number_notes = number_notes + 1.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_scan_statistics~get_number_errors.
    result = number_errors.
  ENDMETHOD.


  METHOD y_if_scan_statistics~get_number_pseudo_comments.
    result = number_pseudo_comments.
  ENDMETHOD.


  METHOD y_if_scan_statistics~get_number_warnings.
    result = number_warnings.
  ENDMETHOD.


  METHOD y_if_scan_statistics~get_number_notes.
    result = number_notes.
  ENDMETHOD.


  METHOD y_if_scan_statistics~increment_pseudo_comment_cnt.
    number_pseudo_comments = number_pseudo_comments + 1.
  ENDMETHOD.
ENDCLASS.

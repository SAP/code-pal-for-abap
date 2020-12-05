INTERFACE y_if_scan_statistics
  PUBLIC .


  METHODS collect
    IMPORTING
      !kind TYPE sychar01
      !pc   TYPE sychar01 .
  METHODS get_number_errors
    RETURNING
      VALUE(result) TYPE i .
  METHODS get_number_warnings
    RETURNING
      VALUE(result) TYPE i .
  METHODS get_number_notes
    RETURNING
      VALUE(result) TYPE i .
  METHODS get_number_pseudo_comments
    RETURNING
      VALUE(result) TYPE i .
  METHODS increment_pseudo_comment_cnt .
ENDINTERFACE.

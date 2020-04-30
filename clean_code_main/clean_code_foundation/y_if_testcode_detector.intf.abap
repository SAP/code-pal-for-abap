INTERFACE y_if_testcode_detector
  PUBLIC .


  TYPES:
    BEGIN OF t_test_code,
      class  TYPE seoclsname,
      method TYPE seocmpname,
    END OF t_test_code .
  TYPES:
    t_test_codes TYPE STANDARD TABLE OF t_test_code WITH DEFAULT KEY .

  METHODS clear .
  METHODS is_testcode
    IMPORTING
      !structure    TYPE sstruc
    RETURNING
      VALUE(result) TYPE abap_bool .
  METHODS set_ref_scan_manager
    IMPORTING
      !ref_scan_manager TYPE REF TO y_if_scan_manager .
ENDINTERFACE.

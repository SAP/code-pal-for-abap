INTERFACE y_if_scan_manager PUBLIC.

  DATA levels TYPE slevel_tab READ-ONLY.
  DATA structures TYPE sstruc_tab READ-ONLY.
  DATA statements TYPE sstmnt_tab READ-ONLY.
  DATA tokens TYPE stokesx_tab READ-ONLY.

  METHODS is_scan_ok RETURNING VALUE(result) TYPE abap_bool.

  METHODS set_ref_scan IMPORTING io_ref_scan TYPE REF TO cl_ci_scan.

ENDINTERFACE.

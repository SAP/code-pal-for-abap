INTERFACE y_if_scan_manager PUBLIC .
  METHODS get_levels RETURNING VALUE(result) TYPE slevel_tab .
  METHODS get_statements RETURNING VALUE(result) TYPE sstmnt_tab .
  METHODS get_structures RETURNING VALUE(result) TYPE sstruc_tab .
  METHODS get_tokens RETURNING VALUE(result) TYPE stokesx_tab .
  METHODS is_scan_ok RETURNING VALUE(result) TYPE abap_bool .
  METHODS set_ref_scan IMPORTING !io_ref_scan TYPE REF TO cl_ci_scan .
ENDINTERFACE.

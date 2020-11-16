INTERFACE y_if_code_pal_sorter PUBLIC.

  CLASS-METHODS get_position IMPORTING check         TYPE sci_chk
                             RETURNING VALUE(result) TYPE synum03
                             RAISING   cx_sy_itab_line_not_found.

ENDINTERFACE.

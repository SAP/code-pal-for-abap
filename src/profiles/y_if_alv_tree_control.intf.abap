INTERFACE y_if_alv_tree_control
  PUBLIC .
  METHODS list_control
    RETURNING
      VALUE(result) TYPE REF TO y_if_list.

  METHODS set_field_visibility
    IMPORTING
      !fieldname  TYPE tabname
      !is_visible TYPE abap_bool DEFAULT abap_true.

  METHODS set_field_header_text
    IMPORTING
      fieldname   TYPE tabname
      header_text TYPE lvc_s_fcat-coltext.

  METHODS refresh_display.

  METHODS init_display.

  METHODS to_focus.

  METHODS get_selected_line
    RETURNING VALUE(result) TYPE REF TO data
    RAISING   ycx_entry_not_found.

  METHODS get_selected_index
    RETURNING VALUE(result) TYPE i
    RAISING   ycx_entry_not_found.

  METHODS set_selected_index
    IMPORTING index TYPE i.

  METHODS toolbar_control
    RETURNING VALUE(result) TYPE REF TO cl_gui_toolbar
    RAISING   cx_failed.

  METHODS activate_toolbar
    RAISING cx_failed.

  METHODS deactivate_toolbar
    RAISING cx_failed.
ENDINTERFACE.

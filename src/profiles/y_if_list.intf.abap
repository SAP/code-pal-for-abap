INTERFACE y_if_list
  PUBLIC .
  METHODS append
    IMPORTING line TYPE any.

  METHODS delete_all.

  METHODS delete_at
    IMPORTING index TYPE i.

  METHODS get_line_at
    IMPORTING index         TYPE i
    RETURNING VALUE(result) TYPE REF TO data.

  METHODS insert_at
    IMPORTING line  TYPE any
              index TYPE i.

  METHODS modify_at
    IMPORTING line  TYPE any
              index TYPE i.

  METHODS number_of_rows
    RETURNING VALUE(result) TYPE i.

  METHODS is_contained
    IMPORTING line          TYPE any
    RETURNING VALUE(result) TYPE abap_bool.

  METHODS set_table
    IMPORTING table TYPE STANDARD TABLE.

  METHODS get_table
    RETURNING VALUE(result) TYPE REF TO data.

  METHODS get_type_name
    RETURNING VALUE(result) TYPE string.

  METHODS get_line_index
    IMPORTING line TYPE any
    RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

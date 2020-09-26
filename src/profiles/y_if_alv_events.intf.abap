INTERFACE y_if_alv_events
  PUBLIC .

  TYPES: BEGIN OF simple_event,
           eventid    TYPE int4,
           appl_event TYPE char1,
         END OF simple_event.
  TYPES simple_events TYPE STANDARD TABLE OF simple_event WITH DEFAULT KEY.

  CONSTANTS mode_double_click TYPE i VALUE 1.
  CONSTANTS mode_selection_changed TYPE i VALUE 2.

  METHODS get_events
    IMPORTING mode          TYPE i DEFAULT mode_double_click
    RETURNING VALUE(result) TYPE simple_events.

  METHODS handle_function_selected
              FOR EVENT function_selected OF cl_gui_toolbar
    IMPORTING fcode.

  METHODS handle_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree_simple.

  METHODS handle_selection_changed
      FOR EVENT selection_changed OF cl_gui_alv_tree_simple.

  METHODS register_handler_to_alv_tree
    IMPORTING alv_tree TYPE REF TO cl_gui_alv_tree_simple.

  METHODS register_handler_to_toolbar
    IMPORTING toolbar TYPE REF TO cl_gui_toolbar.
ENDINTERFACE.

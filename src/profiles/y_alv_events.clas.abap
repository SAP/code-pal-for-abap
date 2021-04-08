CLASS y_alv_events DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES y_if_alv_events .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS y_alv_events IMPLEMENTATION.


  METHOD y_if_alv_events~get_events.
    IF mode = y_if_alv_events~mode_double_click.
      result = VALUE y_if_alv_events=>simple_events( ( eventid = cl_gui_column_tree=>eventid_node_double_click
                                                       appl_event = abap_true ) ).
    ELSEIF mode = y_if_alv_events~mode_selection_changed.
      result = VALUE y_if_alv_events=>simple_events( ( eventid = cl_gui_column_tree=>eventid_selection_changed
                                                       appl_event = abap_true ) ).
    ENDIF.
  ENDMETHOD.


  METHOD y_if_alv_events~handle_double_click.
    RETURN.
  ENDMETHOD.


  METHOD y_if_alv_events~handle_function_selected.
    RETURN.
  ENDMETHOD.


  METHOD y_if_alv_events~handle_selection_changed.
    RETURN.
  ENDMETHOD.


  METHOD y_if_alv_events~register_handler_to_alv_tree.
    SET HANDLER y_if_alv_events~handle_double_click FOR alv_tree.
    SET HANDLER y_if_alv_events~handle_selection_changed FOR alv_tree.
  ENDMETHOD.


  METHOD y_if_alv_events~register_handler_to_toolbar.
    SET HANDLER y_if_alv_events~handle_function_selected FOR toolbar.
  ENDMETHOD.
ENDCLASS.

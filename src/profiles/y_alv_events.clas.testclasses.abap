CLASS lcl_unit_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_if_alv_events.
    METHODS setup.

    METHODS get_events_double_klick FOR TESTING.
    METHODS get_events_selection_changed FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_alv_events( ).
  ENDMETHOD.

  METHOD get_events_double_klick.
    cl_abap_unit_assert=>assert_equals( exp  = VALUE y_if_alv_events=>simple_events( ( eventid = cl_gui_column_tree=>eventid_node_double_click
                                                                                      appl_event = abap_true ) )
                                        act  = cut->get_events( y_if_alv_events=>mode_double_click )
                                        msg  = 'Get_Events: Node Double Klick Error! Tables are not equal!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD get_events_selection_changed.
    cl_abap_unit_assert=>assert_equals( exp  = VALUE y_if_alv_events=>simple_events( ( eventid = cl_gui_column_tree=>eventid_selection_changed
                                                                                       appl_event = abap_true ) )
                                        act  = cut->get_events( y_if_alv_events=>mode_selection_changed )
                                        msg  = 'Error in get_events: Node Selection Changed Error! Tables are not equal!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

ENDCLASS.

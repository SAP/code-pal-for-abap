CLASS lcl_unit_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_if_alv_events.
    METHODS setup.

    METHODS get_events_double_klick FOR TESTING.
    METHODS get_events_selection_changed FOR TESTING.
    METHODS register_handler_to_alv_tree FOR TESTING.
    METHODS register_handler_to_toolbar FOR TESTING.
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

  METHOD register_handler_to_alv_tree.
    DATA alv_tree TYPE REF TO cl_gui_alv_tree_simple.
    alv_tree = NEW cl_gui_alv_tree_simple( i_parent = NEW cl_gui_docking_container( ) ).
    cut->register_handler_to_alv_tree( alv_tree ).

    SET HANDLER cut->handle_double_click FOR alv_tree ACTIVATION space.
    cl_abap_unit_assert=>assert_equals( exp  = 0
                                        act  = sy-subrc
                                        msg  = 'Failed to register a handler to the alv tree: handler for double click, not found!'
                                        quit = if_aunit_constants=>quit-no ).

    SET HANDLER cut->handle_selection_changed FOR alv_tree ACTIVATION space.
    cl_abap_unit_assert=>assert_equals( exp  = 0
                                        act  = sy-subrc
                                        msg  = 'Failed to register a handler to the alv tree: handler for selection changed, not found!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD register_handler_to_toolbar.
    DATA alv_tree TYPE REF TO cl_gui_alv_tree_simple.
    alv_tree = NEW cl_gui_alv_tree_simple( i_parent = NEW cl_gui_docking_container( ) ).
    alv_tree->get_toolbar_object( IMPORTING er_toolbar = DATA(toolbar) ).
    cut->register_handler_to_toolbar( toolbar ).

    SET HANDLER cut->handle_function_selected FOR toolbar ACTIVATION space.
    cl_abap_unit_assert=>assert_equals( exp  = 0
                                        act  = sy-subrc
                                        msg  = 'Failed to register a handler to the toolbar: handler for function selected, not found!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

ENDCLASS.

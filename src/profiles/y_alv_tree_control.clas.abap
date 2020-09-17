CLASS y_alv_tree_control DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES y_if_alv_tree_control .

    METHODS constructor
      IMPORTING
        !alv_header_text TYPE slis_entry
        !dynpro_nr       TYPE sydynnr
        !sy_repid        TYPE syrepid
        !docking_side    TYPE i DEFAULT cl_gui_docking_container=>align_at_left
        !ratio           TYPE i
        !type_name       TYPE string
        !sort_table      TYPE lvc_t_sort
        !events          TYPE REF TO y_if_alv_events
        !event_mode      TYPE i DEFAULT y_if_alv_events=>mode_double_click
      RAISING
        cx_sy_create_data_error .
  PROTECTED SECTION.
    METHODS set_all_fields_invisible.

    METHODS autosize_all_fields.

    METHODS call_fieldcatalog_merge.

    METHODS get_excluded_toolbars
      RETURNING VALUE(result) TYPE ui_functions.

  PRIVATE SECTION.
    DATA list TYPE REF TO y_if_list.
    DATA: alv_header        TYPE slis_t_listheader,
          sort              TYPE lvc_t_sort,
          fieldcats         TYPE lvc_t_fcat,
          structure_name    TYPE tabname,
          docking_container TYPE REF TO cl_gui_docking_container,
          alv_tree          TYPE REF TO cl_gui_alv_tree_simple.
ENDCLASS.



CLASS y_alv_tree_control IMPLEMENTATION.


  METHOD autosize_all_fields.
    DATA filler TYPE i VALUE 7.
    LOOP AT fieldcats ASSIGNING FIELD-SYMBOL(<line>).
      IF <line>-dd_outlen GE strlen( <line>-coltext ).
        <line>-outputlen = <line>-dd_outlen + filler.
      ELSE.
        <line>-outputlen = strlen( <line>-coltext ) + filler.
      ENDIF.
    ENDLOOP.
    UNASSIGN <line>.
  ENDMETHOD.


  METHOD call_fieldcatalog_merge.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = structure_name
      CHANGING
        ct_fieldcat      = fieldcats.
  ENDMETHOD.


  METHOD constructor.
    list = NEW y_list( type_name ).

    docking_container = NEW cl_gui_docking_container( repid     = sy_repid
                                                      dynnr     = dynpro_nr
                                                      side      = docking_side
                                                      ratio     = ratio ).

    alv_tree = NEW cl_gui_alv_tree_simple( i_parent         = docking_container
                                           i_item_selection = '' ).

    events->register_handler_to_alv_tree( alv_tree ).
    alv_tree->set_registered_events( events->get_events( event_mode ) ).

    alv_header = VALUE slis_t_listheader( ( typ = 'H' info = alv_header_text ) ).

    sort = sort_table.
    structure_name = CONV tabname( type_name ).

    TRY.
        events->register_handler_to_toolbar( y_if_alv_tree_control~toolbar_control( ) ).
      CATCH cx_failed.
    ENDTRY.

    call_fieldcatalog_merge( ).
    set_all_fields_invisible( ).
  ENDMETHOD.


  METHOD get_excluded_toolbars.
    APPEND cl_gui_alv_tree_simple=>mc_fc_calculate TO result.
    APPEND cl_gui_alv_tree_simple=>mc_fc_print_back TO result.
    APPEND cl_gui_alv_tree_simple=>mc_fc_current_variant TO result.
    APPEND cl_gui_alv_tree_simple=>mc_fc_change_hierarchy TO result.
  ENDMETHOD.


  METHOD set_all_fields_invisible.
    LOOP AT fieldcats ASSIGNING FIELD-SYMBOL(<line>).
      <line>-no_out = abap_true.
    ENDLOOP.
    UNASSIGN <line>.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~get_selected_index.
    CHECK y_if_alv_tree_control~list_control( )->get_line_at( 1 ) IS NOT INITIAL.
    DATA index_table TYPE lvc_t_indx.

    IF sy-subrc EQ 0.
      TRY.
          alv_tree->get_selected_nodes( CHANGING ct_index_outtab = index_table ).
          result = index_table[ 1 ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~set_selected_index.
    CHECK y_if_alv_tree_control~list_control( )->get_line_at( 1 ) IS NOT INITIAL.
    DATA index_table TYPE lvc_t_indx.
    APPEND index TO index_table.
    alv_tree->set_selected_nodes(
      EXPORTING
        it_index_outtab         = index_table
      EXCEPTIONS
        cntl_system_error       = 1
        dp_error                = 2
        failed                  = 3
        error_in_node_key_table = 4
        others                  = 5
    ).
    IF SY-SUBRC <> 0.
      BREAK-POINT.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~get_selected_line.
    result = list->get_line_at( y_if_alv_tree_control~get_selected_index( ) ).
  ENDMETHOD.


  METHOD y_if_alv_tree_control~init_display.
    DATA(table) = y_if_alv_tree_control~list_control( )->get_table( ).
    ASSIGN table->* TO FIELD-SYMBOL(<table>).

    autosize_all_fields( ).

    alv_tree->set_table_for_first_display(
      EXPORTING
        it_list_commentary = alv_header
        it_toolbar_excluding = get_excluded_toolbars( )
      CHANGING
        it_sort            = sort
        it_outtab          = <table>
        it_fieldcatalog    = fieldcats ).

    cl_gui_cfw=>flush( ).
    UNASSIGN <table>.
    alv_tree->expand_tree( 1 ).
  ENDMETHOD.


  METHOD y_if_alv_tree_control~list_control.
    result = list.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~refresh_display.
    alv_tree->refresh_table_display( it_sort = sort ).
    alv_tree->expand_tree( 1 ).
  ENDMETHOD.


  METHOD y_if_alv_tree_control~set_field_header_text.
    LOOP AT fieldcats ASSIGNING FIELD-SYMBOL(<line>) WHERE fieldname EQ to_upper( fieldname ).
      <line>-coltext = header_text.
    ENDLOOP.
    UNASSIGN <line>.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~set_field_visibility.
    LOOP AT fieldcats ASSIGNING FIELD-SYMBOL(<line>) WHERE fieldname EQ to_upper( fieldname ).
      <line>-no_out = xsdbool( is_visible EQ abap_false ).
    ENDLOOP.
    UNASSIGN <line>.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~toolbar_control.
    alv_tree->get_toolbar_object( IMPORTING er_toolbar = result EXCEPTIONS OTHERS = 4 ).
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~to_focus.
    cl_gui_control=>set_focus( alv_tree ).
  ENDMETHOD.
ENDCLASS.

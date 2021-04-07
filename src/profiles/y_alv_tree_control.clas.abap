CLASS y_alv_tree_control DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES y_if_alv_tree_control .

    CLASS-METHODS create
      IMPORTING
        alv_header_text TYPE slis_entry
        dynpro_nr       TYPE sydynnr
        sy_repid        TYPE syrepid
        docking_side    TYPE i DEFAULT cl_gui_docking_container=>align_at_left
        ratio           TYPE i
        type_name       TYPE string
        sort_table      TYPE lvc_t_sort
        events          TYPE REF TO y_if_alv_events
        event_mode      TYPE i DEFAULT y_if_alv_events=>mode_double_click
      RETURNING
        VALUE(result)   TYPE REF TO y_if_alv_tree_control
      RAISING
        cx_sy_create_data_error
        cx_failed.

    METHODS constructor
      IMPORTING
        type_name  TYPE string
        sort_table TYPE lvc_t_sort
        alv_tree   TYPE REF TO cl_gui_alv_tree_simple
        alv_header TYPE slis_t_listheader
      RAISING
        cx_sy_create_data_error
        cx_failed.

  PROTECTED SECTION.
    METHODS set_all_fields_invisible.
    METHODS autosize_all_fields.
    METHODS call_fieldcatalog_merge IMPORTING structure_name TYPE tabname.
    METHODS get_excluded_toolbars RETURNING VALUE(result) TYPE ui_functions.

  PRIVATE SECTION.
    DATA list TYPE REF TO y_if_list.
    DATA alv_header TYPE slis_t_listheader.
    DATA sort TYPE lvc_t_sort.
    DATA fieldcats TYPE lvc_t_fcat.
    DATA alv_tree TYPE REF TO cl_gui_alv_tree_simple.

ENDCLASS.



CLASS Y_ALV_TREE_CONTROL IMPLEMENTATION.


  METHOD autosize_all_fields.
    DATA filler TYPE i VALUE 7.
    LOOP AT fieldcats ASSIGNING FIELD-SYMBOL(<line>).
      IF <line>-dd_outlen >= strlen( <line>-coltext ).
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


  METHOD create.
    DATA(docking_container) = NEW cl_gui_docking_container( repid = sy_repid
                                                            dynnr = dynpro_nr
                                                            side  = docking_side
                                                            ratio = ratio ).

    DATA(alv_tree) = NEW cl_gui_alv_tree_simple( i_parent         = docking_container
                                                 i_item_selection = abap_false
                                                 i_no_html_header = abap_true ).

    alv_tree->get_toolbar_object( IMPORTING er_toolbar = DATA(alv_toolbar) ).

    events->register_handler_to_alv_tree( alv_tree ).
    events->register_handler_to_toolbar( alv_toolbar ).

    alv_tree->set_registered_events( events->get_events( event_mode ) ).

    DATA(alv_header) = VALUE slis_t_listheader( ( typ = 'H' info = alv_header_text ) ).

    result = NEW y_alv_tree_control( type_name = type_name
                                     sort_table = sort_table
                                     alv_tree = alv_tree
                                     alv_header = alv_header ).
  ENDMETHOD.


  METHOD constructor.
    list = NEW y_list( type_name ).
    me->alv_tree = alv_tree.
    me->alv_header = alv_header.
    sort = sort_table.
    call_fieldcatalog_merge( CONV #( type_name ) ).
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
    DATA index_table TYPE lvc_t_indx.
    CHECK y_if_alv_tree_control~list_control( )->get_line_at( 1 ) IS NOT INITIAL.
    IF sy-subrc = 0.
      alv_tree->get_selected_nodes( CHANGING ct_index_outtab = index_table ).
      TRY.
          result = index_table[ 1 ].
        CATCH cx_sy_itab_line_not_found.
          RAISE EXCEPTION TYPE ycx_entry_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~set_selected_index.
    DATA index_table TYPE lvc_t_indx.

    CHECK y_if_alv_tree_control~list_control( )->get_line_at( 1 ) IS NOT INITIAL.

    APPEND index TO index_table.

    alv_tree->set_selected_nodes(
      EXPORTING
        it_index_outtab         = index_table
      EXCEPTIONS
        cntl_system_error       = 1
        dp_error                = 2
        failed                  = 3
        error_in_node_key_table = 4
        OTHERS                  = 5
    ).
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
        it_list_commentary   = alv_header
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
    LOOP AT fieldcats ASSIGNING FIELD-SYMBOL(<line>) WHERE fieldname = to_upper( fieldname ).
      <line>-coltext = header_text.
    ENDLOOP.
    UNASSIGN <line>.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~set_field_visibility.
    LOOP AT fieldcats ASSIGNING FIELD-SYMBOL(<line>) WHERE fieldname = to_upper( fieldname ).
      <line>-no_out = xsdbool( is_visible = abap_false ).
    ENDLOOP.
    UNASSIGN <line>.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~toolbar_control.
    alv_tree->get_toolbar_object( IMPORTING er_toolbar = result EXCEPTIONS OTHERS = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~to_focus.
    cl_gui_control=>set_focus( alv_tree ).
  ENDMETHOD.


  METHOD y_if_alv_tree_control~activate_toolbar.
    LOOP AT y_if_alv_tree_control~toolbar_control( )->m_table_button ASSIGNING FIELD-SYMBOL(<button>).
      y_if_alv_tree_control~toolbar_control( )->set_button_state( EXPORTING enabled  = abap_true
                                                                            fcode    = <button>-function
                                                                  EXCEPTIONS OTHERS  = 4 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_failed.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD y_if_alv_tree_control~deactivate_toolbar.
    LOOP AT y_if_alv_tree_control~toolbar_control( )->m_table_button ASSIGNING FIELD-SYMBOL(<button>).
      y_if_alv_tree_control~toolbar_control( )->set_button_state( EXPORTING enabled  = abap_false
                                                                            fcode    = <button>-function
                                                                  EXCEPTIONS OTHERS  = 4 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_failed.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

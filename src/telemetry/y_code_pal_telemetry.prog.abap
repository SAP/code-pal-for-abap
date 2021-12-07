*&---------------------------------------------------------------------*
*& Report y_code_pal_telemetry
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_code_pal_telemetry.

* entry point
START-OF-SELECTION.

* global objects
  DATA container TYPE REF TO cl_gui_custom_container.
  DATA viewer TYPE REF TO cl_gui_chart_engine.

  DATA(ixml) = cl_ixml=>create( ).
  DATA(stream) = ixml->create_stream_factory( ).
  DATA(counters) = NEW y_code_pal_telemetry( )->y_if_code_pal_telemetry~get_counters( ).

  CALL SCREEN '100'.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF container IS INITIAL.
    DATA ixml_content TYPE REF TO if_ixml_document.
    DATA ixml_header TYPE REF TO if_ixml_document.
    DATA xstring TYPE xstring.

    container = NEW #( 'CONTAINER' ).
    viewer = NEW #( container ).

    DATA(ostream) = stream->create_ostream_xstring( xstring ).

    PERFORM create_content USING ixml_content.
    ixml_content->render( ostream ).
    viewer->set_data( xdata = xstring ).

    CLEAR xstring.

    PERFORM create_header USING ixml_header.
    ostream = stream->create_ostream_xstring( xstring ).
    ixml_header->render( ostream ).
    viewer->set_customizing( xdata = xstring ).
  ENDIF.

  viewer->render( ).
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  create_content
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_content USING document TYPE REF TO if_ixml_document.
  document = ixml->create_document( ).

  document->set_encoding( ixml->create_encoding( byte_order = if_ixml_encoding=>co_little_endian
                                                 character_set = 'utf-8' ) ).

  DATA(simplechartdata) = document->create_simple_element( name = 'SimpleChartData'
                                                           parent = document ).

  DATA(categories) = document->create_simple_element( name = 'Categories'
                                                      parent = simplechartdata ).

  LOOP AT counters ASSIGNING FIELD-SYMBOL(<counter>)
  GROUP BY <counter>-check.
    document->create_simple_element( name = 'C'
                                     parent = categories )->if_ixml_node~set_value( CONV #( <counter>-check ) ).
  ENDLOOP.

  DATA(usages) = document->create_simple_element( name = 'Series'
                                                  parent = simplechartdata ).

  usages->set_attribute( name = 'label'
                         value = 'Usages' ).

  DATA(findings) = document->create_simple_element( name = 'Series'
                                                    parent = simplechartdata ).

  findings->set_attribute( name = 'label'
                           value = 'Findings' ).

  LOOP AT counters ASSIGNING <counter>.
    IF <counter>-event = y_if_code_pal_telemetry=>events-usage.
      document->create_simple_element( name = 'S'
                                       parent = usages )->if_ixml_node~set_value( CONV #( <counter>-count ) ).
    ELSE.
      document->create_simple_element( name = 'S'
                                       parent = findings )->if_ixml_node~set_value( CONV #( <counter>-count ) ).
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_header USING document TYPE REF TO if_ixml_document.
  document = ixml->create_document( ).

  document->set_encoding( ixml->create_encoding( byte_order = if_ixml_encoding=>co_little_endian
                                                 character_set = 'utf-8' ) ).

  DATA(root) = document->create_simple_element( name = 'SAPChartCustomizing'
                                                parent = document ).

  root->set_attribute( name = 'version'
                       value = '1.1' ).

  DATA(globalsettings) = document->create_simple_element( name = 'GlobalSettings'
                                                          parent = root ).

  document->create_simple_element( name = 'ChartType'
                                   parent = globalsettings )->if_ixml_node~set_value( 'Bars' ).

  document->create_simple_element( name = 'FileType'
                                   parent = globalsettings )->if_ixml_node~set_value( 'PNG' ).

  document->create_simple_element( name = 'Dimension'
                                   parent = globalsettings )->if_ixml_node~set_value( 'PseudoTwo' ).

  document->create_simple_element( name = 'Width'
                                   parent = globalsettings )->if_ixml_node~set_value( '640' ).

  document->create_simple_element( name = 'Height'
                                   parent = globalsettings )->if_ixml_node~set_value( '360' ).

  document->create_simple_element( name = 'ColorPalette'
                                   parent = globalsettings )->if_ixml_node~set_value( 'Tradeshow' ).

  DATA(default) = document->create_simple_element( name = 'Defaults'
                                                   parent = globalsettings ).

  document->create_simple_element( name = 'FontFamily'
                                   parent = default )->if_ixml_node~set_value( 'Arial' ).

  DATA(elements) = document->create_simple_element( name = 'Elements'
                                                    parent = root ).

  DATA(chartelements) = document->create_simple_element( name = 'ChartElements'
                                                         parent = elements ).

  DATA(title) = document->create_simple_element( name = 'Title'
                                                 parent = chartelements ).

  document->create_simple_element( name = 'Extension'
                                   parent = title )->if_ixml_node~set_value( 'href="sapevent:onclick?Title"' ).

  document->create_simple_element( name = 'Caption'
                                   parent = title )->if_ixml_node~set_value( 'Telemetry' ).

  document->create_simple_element( name = 'AlignToPlot'
                                   parent = title )->if_ixml_node~set_value( 'true' ).

  DATA(text) = document->create_simple_element( name = 'Text'
                                                parent = title ).

  document->create_simple_element( name = 'Style'
                                   parent = text )->if_ixml_node~set_value( 'Bold' ).

  DATA(subtitle) = document->create_simple_element( name = 'Subtitle'
                                                    parent = chartelements ).

  document->create_simple_element( name = 'Caption'
                                   parent = subtitle )->if_ixml_node~set_value( 'code Pal for ABAP' ).

  document->create_simple_element( name = 'AlignToPlot'
                                   parent = subtitle )->if_ixml_node~set_value( 'true' ).

  DATA(charttypes) = document->create_simple_element( name = 'ChartTypes'
                                                      parent = elements ).

  DATA(bars) = document->create_simple_element( name = 'Bars'
                                                parent = charttypes ).

  document->create_simple_element( name = 'Distance'
                                   parent = bars )->if_ixml_node~set_value( '40' ).

  DATA(legend) = document->create_simple_element( name = 'Legend'
                                                  parent = chartelements ).

  document->create_simple_element( name = 'Alignment'
                                   parent = legend )->if_ixml_node~set_value( 'North' ).
ENDFORM.

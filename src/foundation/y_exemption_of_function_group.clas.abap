CLASS y_exemption_of_function_group DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES y_if_exemption_of_objects .

  PRIVATE SECTION.
    METHODS is_table_maintenance_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_configuration_tablegenerate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_rai_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_object_indepenent_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_EXEMPTION_OF_FUNCTION_GROUP IMPLEMENTATION.


  METHOD is_configuration_tablegenerate.
    DATA fugr_name TYPE tfdir-pname.
    DATA fugr_func TYPE i.
    DATA fugr_func_viewframe TYPE i.

    "Handling of ABAP Namespaces
    IF name(1) = '/'.
      FIND FIRST OCCURRENCE OF '/' IN name+1 MATCH OFFSET DATA(l_offset).
      l_offset = l_offset + 2.
      fugr_name = insert( val = name sub = 'SAPL' off = l_offset ).
    ELSE.
      fugr_name = 'SAPL' && name.
    ENDIF.

    SELECT SINGLE COUNT(*) FROM tfdir INTO fugr_func_viewframe
      WHERE pname = fugr_name AND ( funcname LIKE 'VIEWFRAME%' OR funcname LIKE 'VIEWPROC%' ). "#EC CI_BYPASS  "#EC CI_GENBUFF

    IF fugr_func_viewframe = 0 OR sy-subrc = 4.
      RETURN.
    ENDIF.

    SELECT SINGLE COUNT(*) FROM tfdir INTO fugr_func
      WHERE pname = fugr_name.          "#EC CI_BYPASS. "#EC CI_GENBUFF
    IF ( fugr_func = fugr_func_viewframe ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_object_indepenent_generate.
    DATA l_object TYPE sobj_name.
    l_object = name.
    result = y_exemption_general=>create( )->is_object_exempted( object_type = 'FUGR'
                                                                 object_name = name ).
  ENDMETHOD.


  METHOD is_rai_generate.
    CHECK name(4) = '/1RA'.

    FIND FIRST OCCURRENCE OF '/' IN name+1 MATCH OFFSET DATA(l_offset).
    l_offset = l_offset + 2.
    DATA(fugr_name) = insert( val = name sub = 'SAPL' off = l_offset ).

    SELECT SINGLE funcname FROM tfdir INTO @DATA(rai_fugr_func)
      WHERE pname = @fugr_name
      AND NOT ( ( funcname LIKE '%_UPDATE' ) OR ( funcname LIKE '%_INSERT' ) OR ( funcname LIKE '%_RAI_CREATE_API' ) ). "#EC CI_GENBUFF.

    IF sy-subrc = 4.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_table_maintenance_generate.
    SELECT SINGLE area AS object FROM tlibt INTO @DATA(l_area)
      WHERE area = @name AND ( areat = 'Extended Table Maintenance (Generated)' ) ##NO_TEXT. "#EC CI_GENBUFF
    IF sy-subrc = 0.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_exemption_of_objects~is_exempted.
    result = xsdbool( is_table_maintenance_generate( name ) = abap_true OR
                      is_configuration_tablegenerate( name ) = abap_true OR
                      is_object_indepenent_generate(  name ) = abap_true OR
                      is_rai_generate( name ) ).
  ENDMETHOD.
ENDCLASS.

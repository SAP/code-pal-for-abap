CLASS y_exemption_of_function_group DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_exemption_of_object.
    ALIASES is_exempted FOR y_if_exemption_of_object~is_exempted.

  PRIVATE SECTION.
    CLASS-DATA name TYPE sobj_name.

    CLASS-METHODS is_table_maintenance_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_configuration_tablegenerate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_rai_generate RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_exemption_of_function_group IMPLEMENTATION.


  METHOD y_if_exemption_of_object~is_exempted.
    name = object_name.

    result = xsdbool( is_table_maintenance_generate( ) = abap_true OR
                      is_configuration_tablegenerate( ) = abap_true OR
                      is_rai_generate( ) ).
  ENDMETHOD.


  METHOD is_configuration_tablegenerate.
    DATA fugr_name TYPE tfdir-pname.
    DATA fugr_func TYPE i.
    DATA fugr_func_viewframe TYPE i.

    "Handling of ABAP Namespaces
    IF name(1) = '/'.
      FIND FIRST OCCURRENCE OF '/' IN name+1 MATCH OFFSET DATA(l_offset).
      l_offset = l_offset + 2.
      fugr_name = insert( val = name
                          sub = 'SAPL'
                          off = l_offset ).
    ELSE.
      fugr_name =  |'SAPL'{ name }|.
    ENDIF.

    SELECT SINGLE COUNT(*)
    FROM tfdir
    INTO @fugr_func_viewframe
    WHERE pname = @fugr_name
    AND (    funcname LIKE 'VIEWFRAME%'
          OR funcname LIKE 'VIEWPROC%'
          OR funcname LIKE 'TABLEPROC%'
          OR funcname LIKE 'TABLEFRAME%' ). "#EC CI_BYPASS  "#EC CI_GENBUFF

    IF fugr_func_viewframe = 0 OR sy-subrc = 4.
      RETURN.
    ENDIF.

    SELECT SINGLE COUNT(*)
    FROM tfdir
    INTO @fugr_func
    WHERE pname = @fugr_name.          "#EC CI_BYPASS. "#EC CI_GENBUFF

    IF fugr_func = fugr_func_viewframe.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_rai_generate.
    CHECK name(4) = '/1RA'.

    FIND FIRST OCCURRENCE OF '/' IN name+1 MATCH OFFSET DATA(l_offset).
    l_offset = l_offset + 2.
    DATA(fugr_name) = insert( val = name
                              sub = 'SAPL'
                              off = l_offset ).

    SELECT SINGLE funcname
    FROM tfdir
    INTO @DATA(rai_fugr_func)
    WHERE pname = @fugr_name
    AND NOT ( ( funcname LIKE '%_UPDATE' )
              OR ( funcname LIKE '%_INSERT' )
              OR ( funcname LIKE '%_RAI_CREATE_API' ) ). "#EC CI_GENBUFF.

    IF rai_fugr_func IS INITIAL.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_table_maintenance_generate.
    SELECT SINGLE @abap_true
    FROM tlibt
    INTO @result
    WHERE area = @name
    AND areat = 'Extended Table Maintenance (Generated)' ##NO_TEXT. "#EC CI_GENBUFF
  ENDMETHOD.


ENDCLASS.

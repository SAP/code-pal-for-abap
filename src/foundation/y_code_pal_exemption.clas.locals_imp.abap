*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_exemption_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_exemption.
    ALIASES is_exempt FOR y_if_code_pal_exemption~is_exempt.
    METHODS constructor IMPORTING database_access TYPE REF TO y_code_pal_database_access.

  PROTECTED SECTION.
    DATA database_access TYPE REF TO y_code_pal_database_access.
    DATA object_type TYPE trobjtype.
    DATA object_name TYPE sobj_name.

ENDCLASS.



CLASS lcl_exemption_base IMPLEMENTATION.

  METHOD constructor.
    me->database_access = database_access.
  ENDMETHOD.

  METHOD is_exempt.
    me->object_type = object_type.
    me->object_name = object_name.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_exemption_of_clas DEFINITION INHERITING FROM lcl_exemption_base.
  PUBLIC SECTION.
    METHODS is_exempt REDEFINITION.

  PROTECTED SECTION.
    METHODS is_srv_maint_ui_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_odata_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_ecatt_odata_test_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_fin_infotype_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_extensibility_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_shma_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_proxy_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_sadl_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_exit_class RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_exception_class RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_bcp_application RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA class_header_data TYPE seoclassdf.

ENDCLASS.



CLASS lcl_exemption_of_clas IMPLEMENTATION.

  METHOD is_exempt.
    super->is_exempt( object_type = object_type
                      object_name = object_name ).

    result = abap_true.

    SELECT SINGLE *
    FROM seoclassdf
    INTO @class_header_data
    WHERE clsname = @object_name
    AND version = 1.

    IF sy-subrc = 0.
      result = xsdbool( is_srv_maint_ui_generate( )
                     OR is_odata_generate( )
                     OR is_ecatt_odata_test_generate( )
                     OR is_fin_infotype_generate( )
                     OR is_extensibility_generate( )
                     OR is_shma_generate( )
                     OR is_proxy_generate( )
                     OR is_sadl_generate( )
                     OR is_exit_class( )
                     OR is_exception_class( )
                     OR is_bcp_application( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD is_bcp_application.
    DATA it_bsp_classes TYPE STANDARD TABLE OF seoclsname.

    it_bsp_classes = VALUE #( ( 'CL_BSP_WD_COMPONENT_CONTROLLER' )
                              ( 'CL_BSP_WD_CONTEXT' )
                              ( 'CL_BSP_WD_CONTEXT_NODE' )
                              ( 'CL_BSP_WD_WINDOW' )
                              ( 'CL_BSP_WD_CUSTOM_CONTROLLER' )
                              ( 'CL_BSP_WD_VIEW_CONTROLLER' )
                              ( 'CL_BSP_WD_ADVSEARCH_CONTROLLER' )
                              ( 'CL_BSP_WD_CONTEXT_NODE_ASP' ) ).

    SELECT SINGLE refclsname FROM seometarel
      WHERE clsname = @class_header_data-clsname AND refclsname IS NOT NULL
      INTO @DATA(inherited_by).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DO.
      IF line_exists( it_bsp_classes[ table_line = inherited_by ] ).
        result = abap_true.
        RETURN.
      ENDIF.

      SELECT SINGLE refclsname FROM seometarel
        WHERE clsname = @inherited_by AND refclsname IS NOT NULL
        INTO @inherited_by.
      IF sy-subrc = 4.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD is_ecatt_odata_test_generate.
    SELECT SINGLE @abap_true
    FROM seoclassdf
    INTO @result
    WHERE clsname = @class_header_data-clsname
    AND author = 'eCATTClassGe'.
  ENDMETHOD.

  METHOD is_exception_class.
    CONSTANTS exception_clase_type LIKE class_header_data-category VALUE '40'.
    IF class_header_data-category = exception_clase_type.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_exit_class.
    CONSTANTS exit_class_type LIKE class_header_data-category VALUE '01'.
    IF class_header_data-category = exit_class_type.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_extensibility_generate.
    DATA lt_interfaces TYPE seor_implementing_keys.
    DATA lv_seoclskey TYPE  seoclskey.

    lv_seoclskey = class_header_data-clsname.

    CALL FUNCTION 'SEO_CLASS_ALL_IMPLEMENTG_GET'
      EXPORTING
        clskey       = lv_seoclskey
      IMPORTING
        set          = lt_interfaces
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = xsdbool( line_exists( lt_interfaces[ refclsname = 'IF_CFD_ODATA_MPC_FLX' ] )
                   OR line_exists( lt_interfaces[ refclsname = 'IF_CFD_ODATA_DPC_FLX' ] ) ).
  ENDMETHOD.

  METHOD is_fin_infotype_generate.
    SELECT SINGLE @abap_true
    FROM t777ditclass
    INTO @result
    WHERE idclass = @class_header_data-clsname
    OR cont_db = @class_header_data-clsname
    OR bl_class = @class_header_data-clsname.
  ENDMETHOD.

  METHOD is_odata_generate.
    SELECT SINGLE @abap_true
    FROM /iwbep/i_sbd_ga
    INTO @result
    WHERE ( gen_art_type = 'DPCB' OR gen_art_type = 'MPCB' )
    AND trobj_type = 'CLAS'
    AND trobj_name = @class_header_data-clsname. "#EC CI_NOFIELD
  ENDMETHOD.

  METHOD is_proxy_generate.
    result = xsdbool( class_header_data-clsproxy = abap_true ).
  ENDMETHOD.

  METHOD is_sadl_generate.
    SELECT SINGLE @abap_true
    FROM seoclasstx
    INTO @result
    WHERE clsname = @class_header_data-clsname
    AND descript = 'Generated by SADL Generation Toolkit' ##NO_TEXT.
  ENDMETHOD.

  METHOD is_shma_generate.
    SELECT SINGLE @abap_true
    FROM tadir
    INTO @result
    WHERE pgmid = 'R3TR'
    AND object = 'SHMA'
    AND obj_name = @class_header_data-clsname.
  ENDMETHOD.

  METHOD is_srv_maint_ui_generate.
    DATA: lt_interfaces TYPE seor_implementing_keys.
    DATA: lv_seoclskey TYPE seoclskey.
    lv_seoclskey = class_header_data-clsname.

    CALL FUNCTION 'SEO_CLASS_ALL_IMPLEMENTG_GET'
      EXPORTING
        clskey       = lv_seoclskey
      IMPORTING
        set          = lt_interfaces
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = xsdbool( line_exists( lt_interfaces[ refclsname = '/FTI/IF_FTI_MODEL' ] )
                   OR line_exists( lt_interfaces[ refclsname = '/FTI/IF_NATIVE_SQL_GENERATOR' ] ) ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_exemption_of_fugr DEFINITION INHERITING FROM lcl_exemption_base.
  PUBLIC SECTION.
    METHODS is_exempt REDEFINITION.

  PROTECTED SECTION.
    METHODS is_table_maintenance_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_configuration_tablegenerate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_rai_generate RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS lcl_exemption_of_fugr IMPLEMENTATION.

  METHOD is_exempt.
    super->is_exempt( object_type = object_type
                      object_name = object_name ).

    result = xsdbool( is_table_maintenance_generate( )
                   OR is_configuration_tablegenerate( )
                   OR is_rai_generate( ) ).
  ENDMETHOD.

  METHOD is_configuration_tablegenerate.
    DATA fugr_name TYPE tfdir-pname.
    DATA fugr_func TYPE i.
    DATA fugr_func_viewframe TYPE i.

    "Handling of ABAP Namespaces
    IF object_name(1) = '/'.
      FIND FIRST OCCURRENCE OF '/' IN object_name+1 MATCH OFFSET DATA(l_offset).
      l_offset = l_offset + 2.
      fugr_name = insert( val = object_name
                          sub = 'SAPL'
                          off = l_offset ).
    ELSE.
      fugr_name =  |'SAPL'{ object_name }|.
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
    CHECK object_name(4) = '/1RA'.

    FIND FIRST OCCURRENCE OF '/' IN object_name+1 MATCH OFFSET DATA(l_offset).
    l_offset = l_offset + 2.
    DATA(fugr_name) = insert( val = object_name
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
    WHERE area = @object_name
    AND areat = 'Extended Table Maintenance (Generated)' ##NO_TEXT. "#EC CI_GENBUFF
  ENDMETHOD.

ENDCLASS.


CLASS lcl_exemption_of_prog DEFINITION INHERITING FROM lcl_exemption_base.
  PUBLIC SECTION.
    METHODS is_exempt REDEFINITION.

  PROTECTED SECTION.
    METHODS is_enterprise_search_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_downport_assist_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_fin_infotyp_generate RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_object_sw01_generate RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS lcl_exemption_of_prog IMPLEMENTATION.

  METHOD is_exempt.
    super->is_exempt( object_type = object_type
                      object_name = object_name ).

    result = xsdbool( is_enterprise_search_generate( )
                   OR is_downport_assist_generate( )
                   OR is_fin_infotyp_generate( )
                   OR is_object_sw01_generate( ) ).
  ENDMETHOD.

  METHOD is_downport_assist_generate.
    IF object_name(5) = 'NOTE_'.
      result = abap_true.
    ELSE.
      FIND REGEX '_NOTE_\d{1,20}\>' IN object_name.
      IF sy-subrc = 0.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD is_enterprise_search_generate.
    SELECT SINGLE @abap_true
    FROM trdir
    INTO @result
    WHERE name LIKE '%\_001' ESCAPE '\'
    AND ( secu = 'ESH' OR name LIKE 'ESHS%' )
    AND ( subc = 'S' OR subc = '1' )     "include programs ('I') are not supported
    AND name = @object_name.
  ENDMETHOD.

  METHOD is_fin_infotyp_generate.
    result = database_access->is_infotype( object_name ).
  ENDMETHOD.

  METHOD is_object_sw01_generate.
    result = database_access->is_business_object( object_name ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_exemption_general DEFINITION INHERITING FROM lcl_exemption_base.
  PUBLIC SECTION.
    METHODS is_exempt REDEFINITION.

  PROTECTED SECTION.
    METHODS is_tadir_generated RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_object_existing RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_exemption_object RETURNING VALUE(result) TYPE REF TO y_if_code_pal_exemption.

ENDCLASS.



CLASS lcl_exemption_general IMPLEMENTATION.

  METHOD is_exempt.
    super->is_exempt( object_type = object_type
                      object_name = object_name ).

    result = xsdbool( is_object_existing( )
                   OR is_tadir_generated( )
                   OR get_exemption_object( )->is_exempt( object_type = object_type
                                                          object_name = object_name ) ).
  ENDMETHOD.

  METHOD get_exemption_object.
    result = COND #( WHEN object_type = 'PROG' THEN NEW lcl_exemption_of_prog( database_access )
                     WHEN object_type = 'CLAS' THEN NEW lcl_exemption_of_clas( database_access )
                     WHEN object_type = 'FUGR' THEN NEW lcl_exemption_of_fugr( database_access ) ).
  ENDMETHOD.

  METHOD is_object_existing.
    "TODO: Review it. It is not supported in the RFC scenario.
    CONSTANTS object_exists TYPE char1 VALUE 'X'.

    DATA existence_flag TYPE strl_pari-flag.
    DATA l_object_type  TYPE e071-object.
    DATA l_object_name  TYPE e071-obj_name.

    l_object_type = object_type.
    l_object_name = object_name.

    CALL FUNCTION 'TR_CHECK_EXIST'
      EXPORTING
        iv_pgmid             = 'R3TR'
        iv_object            = l_object_type
        iv_obj_name          = l_object_name
      IMPORTING
        e_exist              = existence_flag
      EXCEPTIONS
        tr_no_check_function = 1.

    IF sy-subrc = 0 AND existence_flag <> object_exists.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_tadir_generated.
    result = database_access->get_object_generated( object_type = object_type
                                                    object_name = object_name ).
  ENDMETHOD.

ENDCLASS.

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
    METHODS is_bsp_application IMPORTING class LIKE object_name
                               RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA class_header_data TYPE seoclassdf.

ENDCLASS.



CLASS lcl_exemption_of_clas IMPLEMENTATION.

  METHOD is_exempt.
    super->is_exempt( object_type = object_type
                      object_name = object_name ).

    result = abap_true.

    DATA(definitions) = database_access->get_class_definition( CONV #( object_name ) ).

    SORT definitions BY version.
    class_header_data = definitions[ 1 ].

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
                   OR is_bsp_application( object_name ) ).
  ENDMETHOD.

  METHOD is_bsp_application.
    DATA it_bsp_classes TYPE STANDARD TABLE OF seoclsname.

    it_bsp_classes = VALUE #( ( 'CL_BSP_WD_COMPONENT_CONTROLLER' )
                              ( 'CL_BSP_WD_CONTEXT' )
                              ( 'CL_BSP_WD_CONTEXT_NODE' )
                              ( 'CL_BSP_WD_WINDOW' )
                              ( 'CL_BSP_WD_CUSTOM_CONTROLLER' )
                              ( 'CL_BSP_WD_VIEW_CONTROLLER' )
                              ( 'CL_BSP_WD_ADVSEARCH_CONTROLLER' )
                              ( 'CL_BSP_WD_CONTEXT_NODE_ASP' ) ).

    DATA(metadatas) = database_access->get_class_metadata( CONV #( object_name ) ).

    IF lines( metadatas ) = 0.
      RETURN.
    ENDIF.

    LOOP AT metadatas ASSIGNING FIELD-SYMBOL(<metadata>).
      result = xsdbool( line_exists( it_bsp_classes[ table_line = <metadata>-refclsname ] )
                     OR is_bsp_application( CONV #( <metadata>-refclsname ) ) ).

      IF result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_ecatt_odata_test_generate.
    database_access->repository_access->get_object_info( EXPORTING i_object_key     = VALUE #( obj_type = object_type
                                                                                               obj_name = object_name )
                                                         IMPORTING e_contact_person = DATA(author) ).
    result = xsdbool( author = 'eCATTClassGe' ).
  ENDMETHOD.

  METHOD is_exception_class.
    CONSTANTS exception_clase_type LIKE class_header_data-category VALUE '40'.
    result = xsdbool( class_header_data-category = exception_clase_type ).
  ENDMETHOD.

  METHOD is_exit_class.
    CONSTANTS exit_class_type LIKE class_header_data-category VALUE '01'.
    result = xsdbool( class_header_data-category = exit_class_type ).
  ENDMETHOD.

  METHOD is_extensibility_generate.
    "TODO: RFC?
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
    DATA(t777ditclass) = database_access->get_hrbas_infotype( CONV #( object_name ) ).
    result = xsdbool( lines( t777ditclass ) > 0 ).
  ENDMETHOD.

  METHOD is_odata_generate.
    DATA(sbd_ga) = database_access->get_service_builder_artifact( object_type = object_type
                                                                  object_name = object_name ).

    result = xsdbool( line_exists( sbd_ga[ gen_art_type = 'DPCB' ] )
                   OR line_exists( sbd_ga[ gen_art_type = 'MPCB' ] ) ).
  ENDMETHOD.

  METHOD is_proxy_generate.
    result = xsdbool( class_header_data-clsproxy = abap_true ).
  ENDMETHOD.

  METHOD is_sadl_generate.
    DATA(description) = database_access->repository_access->get_class_description( CONV #( object_name ) ).
    result = xsdbool( description = 'Generated by SADL Generation Toolkit' ).
  ENDMETHOD.

  METHOD is_shma_generate.
   result = database_access->repository_access->exists_object( VALUE #( obj_type = 'SHMA'
                                                                        obj_name = object_name ) ).
  ENDMETHOD.

  METHOD is_srv_maint_ui_generate.
    "TODO: RFC?
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

    DATA(functions) = database_access->repository_access->get_functions_of_function_pool( CONV #( fugr_name ) ).
    result = abap_true.

    LOOP AT functions TRANSPORTING NO FIELDS
    WHERE funcname NP 'VIEWFRAME*'
    OR funcname NP 'VIEWPROC*'
    OR funcname NP 'TABLEPROC*'
    OR funcname NP 'TABLEFRAME*'.
      result = abap_false.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_rai_generate.
    CHECK object_name(4) = '/1RA'.

    FIND FIRST OCCURRENCE OF '/' IN object_name+1 MATCH OFFSET DATA(l_offset).
    l_offset = l_offset + 2.
    DATA(fugr_name) = insert( val = object_name
                              sub = 'SAPL'
                              off = l_offset ).

    DATA(functions) = database_access->repository_access->get_functions_of_function_pool( CONV #( fugr_name ) ).
    result = abap_true.

    LOOP AT functions TRANSPORTING NO FIELDS
    WHERE funcname NP '*_UPDATE'
    OR funcname NP '*_INSERT'
    OR funcname NP '*_RAI_CREATE_API'.
      result = abap_false.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_table_maintenance_generate.
    DATA(description) = database_access->repository_access->get_function_description( CONV #( object_name ) ).
    result = xsdbool( description = 'Extended Table Maintenance (Generated)' ).
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
    DATA(trdir) = database_access->get_trdir( object_type = object_type
                                              object_name = object_name ).

    LOOP AT trdir TRANSPORTING NO FIELDS
    WHERE name CP '*\_001'
    AND ( secu = 'ESH' OR name CP 'ESHS*' )
    AND ( subc = 'S' OR subc = '1' ).
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_fin_infotyp_generate.
    result = xsdbool( database_access->get_infotype( object_name ) IS NOT INITIAL ).
  ENDMETHOD.

  METHOD is_object_sw01_generate.
    result = xsdbool( database_access->get_table_object_repository( object_name ) IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_exemption_general DEFINITION INHERITING FROM lcl_exemption_base.
  PUBLIC SECTION.
    METHODS is_exempt REDEFINITION.

  PROTECTED SECTION.
    METHODS is_generated RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_exemption_object RETURNING VALUE(result) TYPE REF TO y_if_code_pal_exemption.

ENDCLASS.



CLASS lcl_exemption_general IMPLEMENTATION.

  METHOD is_exempt.
    super->is_exempt( object_type = object_type
                      object_name = object_name ).

    result = xsdbool( is_generated( )
                   OR get_exemption_object( )->is_exempt( object_type = object_type
                                                          object_name = object_name ) ).
  ENDMETHOD.

  METHOD get_exemption_object.
    result = COND #( WHEN object_type = 'PROG' THEN NEW lcl_exemption_of_prog( database_access )
                     WHEN object_type = 'CLAS' THEN NEW lcl_exemption_of_clas( database_access )
                     WHEN object_type = 'FUGR' THEN NEW lcl_exemption_of_fugr( database_access ) ).
  ENDMETHOD.

  METHOD is_generated.
    DATA(tadir) = database_access->get_tadir( object_type = object_type
                                              object_name = object_name ).

    result = xsdbool( line_exists( tadir[ genflag = abap_true ] ) ).
  ENDMETHOD.

ENDCLASS.

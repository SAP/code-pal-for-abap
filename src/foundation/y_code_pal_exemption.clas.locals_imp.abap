*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_exemption_base IMPLEMENTATION.

  METHOD constructor.
    me->database_access = database_access.
    me->object_type = object_type.
    me->object_name = object_name.
    me->include = include.
  ENDMETHOD.


  METHOD is_exempt.
    result = xsdbool( has_tadir_generated_flag( )
                   OR has_trdir_generated_flag( ) ).
  ENDMETHOD.


  METHOD has_tadir_generated_flag.
    DATA(tadir) = database_access->get_tadir( object_type = object_type
                                              object_name = object_name ).

    result = xsdbool( line_exists( tadir[ genflag = abap_true ] ) ).
  ENDMETHOD.


  METHOD has_trdir_generated_flag.
    DATA(trdir) = database_access->get_trdir( include ).

    result = xsdbool( line_exists( trdir[ occurs = abap_true ] ) ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_exemption_of_clas IMPLEMENTATION.

  METHOD is_exempt.
    CHECK super->is_exempt( ) = abap_false.

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

    DATA(metadatas) = database_access->get_class_metadata( CONV #( class_name ) ).

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
    database_access->repository_access->get_object_info( EXPORTING i_object_key     = VALUE #( pgmid = 'R3TR'
                                                                                               obj_type = object_type
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



CLASS lcl_exemption_of_fugr IMPLEMENTATION.

  METHOD is_exempt.
    CHECK super->is_exempt( ) = abap_false.

    result = xsdbool( is_table_maintenance_generate( )
                   OR is_configuration_tablegenerate( )
                   OR is_rai_generate( ) ).
  ENDMETHOD.


  METHOD is_configuration_tablegenerate.
    DATA(functions) = database_access->repository_access->get_functions_of_function_pool( CONV #( object_name ) ).

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
    DATA(functions) = database_access->repository_access->get_functions_of_function_pool( CONV #( object_name ) ).

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



CLASS lcl_exemption_of_prog IMPLEMENTATION.

  METHOD is_exempt.
    CHECK super->is_exempt( ) = abap_false.

    result = xsdbool( is_downport_assist_generate( )
                   OR is_fin_infotyp_generate( )
                   OR is_object_sw01_generate( ) ).
  ENDMETHOD.


  METHOD is_downport_assist_generate.
    result = xsdbool( object_name CP 'NOTE_*'
                   OR object_name CP '_NOTE_*' ).
  ENDMETHOD.


  METHOD is_fin_infotyp_generate.
    result = xsdbool( database_access->get_infotype( object_name ) IS NOT INITIAL ).
  ENDMETHOD.


  METHOD is_object_sw01_generate.
    result = xsdbool( database_access->get_table_object_repository( object_name ) IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_exemption_factory IMPLEMENTATION.

  METHOD get.
    CASE object_type.
      WHEN 'PROG'.
        result = NEW lcl_exemption_of_prog( database_access = database_access
                                            object_type     = object_type
                                            object_name     = object_name
                                            include         = include ).
      WHEN 'CLAS' OR 'INTF'.
        result = NEW lcl_exemption_of_clas( database_access = database_access
                                            object_type     = object_type
                                            object_name     = object_name
                                            include         = include ).
      WHEN 'FUGR'.
        result = NEW lcl_exemption_of_fugr( database_access = database_access
                                            object_type     = object_type
                                            object_name     = object_name
                                            include         = include ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

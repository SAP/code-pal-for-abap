CLASS y_exemption_of_class DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_exemption_of_object.
    ALIASES is_exempted FOR y_if_exemption_of_object~is_exempted.

  PRIVATE SECTION.
    CLASS-DATA class_header_data TYPE seoclassdf.

    CLASS-METHODS is_srv_maint_ui_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_odata_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_ecatt_odata_test_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_fin_infotype_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_extensibility_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_shma_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_proxy_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_sadl_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_exit_class RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_exception_class RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_bcp_application RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_exemption_of_class IMPLEMENTATION.

  METHOD y_if_exemption_of_object~is_exempted.
    result = abap_true.

    SELECT SINGLE *
    FROM seoclassdf
    INTO @class_header_data
    WHERE clsname = @object_name
    AND version = 1.

    IF sy-subrc = 0.
      result = xsdbool( is_srv_maint_ui_generate( ) = abap_true OR
                        is_odata_generate( ) = abap_true OR
                        is_ecatt_odata_test_generate( ) = abap_true OR
                        is_fin_infotype_generate(  ) = abap_true OR
                        is_extensibility_generate(  ) = abap_true OR
                        is_shma_generate(  ) = abap_true OR
                        is_proxy_generate(  ) = abap_true OR
                        is_sadl_generate(  ) = abap_true OR
                        is_exit_class(  ) = abap_true OR
                        is_exception_class(  ) = abap_true OR
                        is_bcp_application( ) = abap_true ).
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

    "Multi Inheritance
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
    DATA: lt_interfaces TYPE seor_implementing_keys.
    DATA: lv_seoclskey TYPE  seoclskey.

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

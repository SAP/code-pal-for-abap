CLASS y_code_pal_database_access DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_database_access.
    METHODS constructor IMPORTING srcid TYPE scr_source_id.

  PROTECTED SECTION.
    ALIASES repository_access FOR y_if_code_pal_database_access~repository_access.

  PRIVATE SECTION.
    DATA rfc_destination TYPE rfcdest.

ENDCLASS.



CLASS y_code_pal_database_access IMPLEMENTATION.

  METHOD constructor.
    IF srcid IS NOT INITIAL.
      "Remote
      rfc_destination = cl_abap_source_id=>get_destination( srcid ).
      repository_access = cl_sca_repository_access=>get_access_by_rfc( rfc_destination ).
    ELSE.
      "Local
      repository_access = cl_sca_repository_access=>get_local_access( ).
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_tadir.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'TADIR' ).

    sql->add_where( |PGMID = 'R3TR'| ).
    sql->add_where( |AND OBJECT = '{ object_type }'| ).
    sql->add_where( |AND OBJ_NAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_table_object_repository.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'TOJTB' ).

    sql->add_where( |PROGNAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_source_code.
    DATA(source) = NEW lcl_report_source( rfc_destination = rfc_destination
                                          object_type     = object_type
                                          object_name     = CONV #( object_name ) ).

    source->run( ).

    result = source->get_source_code( ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_infotype.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'T777D' ).

    sql->add_where( |REPID = '{ object_name }'| ).
    sql->add_where( |OR BTCI_PROG = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_trdir.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'TRDIR' ).

    sql->add_where( |NAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_service_builder_artifact.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( '/IWBEP/I_SBD_GA' ).

    sql->add_where( |TROBJ_TYPE = '{ object_type }'| ).
    sql->add_where( |AND TROBJ_NAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_hrbas_infotype.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'T777DITCLASS' ).

    sql->add_where( |IDCLASS = '{ object_name }'| ).
    sql->add_where( |OR CONT_DB = '{ object_name }'| ).
    sql->add_where( |OR BL_CLASS = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_class_metadata.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'SEOMETAREL' ).

    sql->add_where( |CLSNAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_class_definition.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'SEOCLASSDF' ).

    sql->add_where( |CLSNAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_function_attributes.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'ENLFDIR' ).

    sql->add_where( |FUNCNAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_enhancement_spot.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'ENHSPOTOBJ' ).

    sql->add_where( |OBJ_TYPE = '{ object_type }'| ).
    sql->add_where( |AND OBJ_NAME = '{ object_name }'| ).
    sql->add_where( |AND VERSION = 'A'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_view_maintenance_routines.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'TVIMF' ).

    sql->add_where( |FORMNAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_function_module.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'TFDIR' ).

    sql->add_where( |FUNCNAME = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.


  METHOD y_if_code_pal_database_access~get_message_class.
    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'T100A' ).

    sql->add_where( |ARBGB = '{ object_name }'| ).

    sql->run( CHANGING table = result ).
  ENDMETHOD.

ENDCLASS.

CLASS y_code_pal_database_access DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES tty_source_code TYPE TABLE OF abaptxt255 WITH EMPTY KEY.
    TYPES tty_tadir TYPE TABLE OF tadir WITH DEFAULT KEY.
    TYPES tty_trdir TYPE TABLE OF trdir WITH DEFAULT KEY.
    TYPES tty_tojtb TYPE TABLE OF tojtb WITH DEFAULT KEY.
    TYPES tty_t777d TYPE TABLE OF t777d WITH DEFAULT KEY.
    TYPES tty_sbd_ga TYPE TABLE OF /iwbep/i_sbd_ga WITH DEFAULT KEY.
    TYPES tty_t777ditclass TYPE TABLE OF t777ditclass WITH DEFAULT KEY.
    TYPES tty_seometarel TYPE TABLE OF seometarel WITH DEFAULT KEY.
    TYPES tty_seoclassdf TYPE TABLE OF seoclassdf WITH DEFAULT KEY.
    TYPES tty_reposrc TYPE TABLE OF reposrc WITH DEFAULT KEY.
    TYPES tty_vrsd TYPE TABLE OF vrsd WITH DEFAULT KEY.

    DATA repository_access TYPE REF TO if_sca_repository_access READ-ONLY.

    METHODS constructor IMPORTING srcid TYPE scr_source_id.

    METHODS get_tadir IMPORTING object_type   TYPE tadir-object
                                object_name   TYPE tadir-obj_name
                      RETURNING VALUE(result) TYPE tty_tadir.

    METHODS get_table_object_repository IMPORTING object_name   TYPE tojtb-progname
                                        RETURNING VALUE(result) TYPE tty_tojtb.

    METHODS get_source_code IMPORTING object_type   TYPE tadir-object
                                      object_name   TYPE tadir-obj_name
                            RETURNING VALUE(result) TYPE tty_source_code.

    METHODS get_trdir IMPORTING object_type   TYPE tadir-object
                                object_name   TYPE tadir-obj_name
                      RETURNING VALUE(result) TYPE tty_trdir.

    METHODS get_infotype IMPORTING object_name   TYPE t777d-repid
                         RETURNING VALUE(result) TYPE tty_t777d.

    METHODS get_service_builder_artifact IMPORTING object_type   TYPE /iwbep/i_sbd_ga-trobj_type
                                                   object_name   TYPE /iwbep/i_sbd_ga-trobj_name
                                         RETURNING VALUE(result) TYPE tty_sbd_ga.

    METHODS get_hrbas_infotype IMPORTING object_name   TYPE t777ditclass-idclass
                               RETURNING VALUE(result) TYPE tty_t777ditclass.

    METHODS get_class_metadata IMPORTING object_name   TYPE seometarel-clsname
                               RETURNING VALUE(result) TYPE tty_seometarel.

    METHODS get_class_definition IMPORTING object_name   TYPE seoclassdf-clsname
                                 RETURNING VALUE(result) TYPE tty_seoclassdf.

    METHODS get_report_source IMPORTING object_name   TYPE reposrc-progname
                              RETURNING VALUE(result) TYPE tty_reposrc.

    METHODS get_version_management IMPORTING object_type   TYPE vrsd-objtype
                                             object_name   TYPE vrsd-objname
                                   RETURNING VALUE(result) TYPE tty_vrsd.



  PRIVATE SECTION.
    DATA rfc_destination TYPE rfcdest.

ENDCLASS.



CLASS y_code_pal_database_access IMPLEMENTATION.

  METHOD constructor.
    repository_access = COND #( WHEN srcid IS INITIAL THEN cl_sca_repository_access=>get_local_access( )
                                                      ELSE cl_sca_repository_access=>get_access_by_rfc( cl_abap_source_id=>get_destination( srcid ) ) ).
  ENDMETHOD.

  METHOD get_tadir.
    DATA tadir TYPE tty_tadir.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'TADIR' ).

    sql->add_where( |PGMID = 'R3TR'| ).
    sql->add_where( |AND OBJECT = '{ object_type }'| ).
    sql->add_where( |AND OBJ_NAME = '{ object_name }'| ).

    IF sql->run( CHANGING table = tadir ).
      result = tadir.
    ENDIF.
  ENDMETHOD.

  METHOD get_table_object_repository.
    DATA tojtb TYPE tty_tojtb.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'TOJTB' ).

    sql->add_where( |PROGNAME = '{ object_name }'| ).

    IF sql->run( CHANGING table = tojtb ).
      result = tojtb.
    ENDIF.
  ENDMETHOD.

  METHOD get_source_code.
    DATA(source) = NEW lcl_report_source( rfc_destination = rfc_destination
                                          object_type     = object_type
                                          object_name     = object_name ).

    source->run( ).

    result = source->get_source_code( ).
  ENDMETHOD.

  METHOD get_infotype.
    DATA t777d TYPE tty_t777d.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'T777D' ).

    sql->add_where( |REPID = '{ object_name }'| ).
    sql->add_where( |OR BTCI_PROG = '{ object_name }'| ).

    IF sql->run( CHANGING table = t777d ).
      result = t777d.
    ENDIF.
  ENDMETHOD.

  METHOD get_trdir.
    DATA(source) = NEW lcl_report_source( rfc_destination = rfc_destination
                                          object_type     = object_type
                                          object_name     = object_name ).

    source->run( ).

    result = source->get_trdir( ).
  ENDMETHOD.

  METHOD get_service_builder_artifact.
    DATA sbd_ga TYPE tty_sbd_ga.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( '/IWBEP/I_SBD_GA' ).

    sql->add_where( |TROBJ_TYPE = '{ object_type }'| ).
    sql->add_where( |AND TROBJ_NAME = '{ object_name }'| ).

    IF sql->run( CHANGING table = sbd_ga ).
      result = sbd_ga.
    ENDIF.
  ENDMETHOD.

  METHOD get_hrbas_infotype.
    DATA t777ditclass TYPE tty_t777ditclass.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'T777DITCLASS' ).

    sql->add_where( |IDCLASS = '{ object_name }'| ).
    sql->add_where( |OR CONT_DB = '{ object_name }'| ).
    sql->add_where( |OR BL_CLASS = '{ object_name }'| ).

    IF sql->run( CHANGING table = t777ditclass ).
      result = t777ditclass.
    ENDIF.
  ENDMETHOD.

  METHOD get_class_metadata.
    DATA seometarel TYPE tty_seometarel.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'SEOMETAREL' ).

    sql->add_where( |CLSNAME = '{ object_name }'| ).

    IF sql->run( CHANGING table = seometarel ).
      result = seometarel.
    ENDIF.
  ENDMETHOD.

  METHOD get_class_definition.
    DATA seoclassdf TYPE tty_seoclassdf.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'SEOCLASSDF' ).

    sql->add_where( |CLSNAME = '{ object_name }'| ).

    IF sql->run( CHANGING table = seoclassdf ).
      result = seoclassdf.
    ENDIF.
  ENDMETHOD.

  METHOD get_report_source.
    DATA reposrc TYPE tty_reposrc.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'REPOSRC' ).

    sql->add_where( |PROGNAME = '{ object_name }'| ).
    sql->add_where( |AND R3STATE = 'A'| ).

    IF sql->run( CHANGING table = reposrc ).
      result = reposrc.
    ENDIF.
  ENDMETHOD.

  METHOD get_version_management.
    DATA vrsd TYPE tty_vrsd.

    DATA(sql) = NEW lcl_select( rfc_destination ).

    sql->set_from( 'VRSD' ).

    sql->add_where( |OBJTYPE = '{ object_type }'| ).
    sql->add_where( |AND OBJNAME LIKE '{ object_name }'| ).
    sql->add_where( |AND DATUM IS NOT NULL| ).

    IF sql->run( CHANGING table = vrsd ).
      result = vrsd.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

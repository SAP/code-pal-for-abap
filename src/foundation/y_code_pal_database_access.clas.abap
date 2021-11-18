CLASS y_code_pal_database_access DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES ty_source_code TYPE TABLE OF abaptxt255 WITH EMPTY KEY.
    TYPES ty_trdir TYPE TABLE OF trdir WITH DEFAULT KEY.

    DATA repository_access TYPE REF TO if_sca_repository_access READ-ONLY.
    DATA repository_proxy  TYPE REF TO if_sca_repository_proxy READ-ONLY.

    METHODS constructor IMPORTING srcid TYPE scr_source_id.

    METHODS get_class_inheritance IMPORTING clsname       TYPE seometarel-clsname
                                  RETURNING VALUE(result) TYPE seometarel-refclsname.

    METHODS get_object_generated IMPORTING object_type   TYPE tadir-object
                                           object_name   TYPE tadir-obj_name
                                 RETURNING VALUE(result) TYPE tadir-genflag.

    METHODS is_business_object IMPORTING object_name   TYPE tojtb-progname
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_source_code IMPORTING object_type   TYPE tadir-object
                                      object_name   TYPE tadir-obj_name
                            RETURNING VALUE(result) TYPE ty_source_code.

    METHODS get_trdir IMPORTING object_type   TYPE tadir-object
                                object_name   TYPE tadir-obj_name
                      RETURNING VALUE(result) TYPE trdir.

    METHODS is_infotype IMPORTING object_name   TYPE t777d-repid
                        RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA destination TYPE rfcdest.

ENDCLASS.



CLASS y_code_pal_database_access IMPLEMENTATION.

  METHOD constructor.
    repository_access = COND #( WHEN srcid IS INITIAL THEN cl_sca_repository_access=>get_local_access( )
                                                      ELSE cl_sca_repository_access=>get_access_by_rfc( cl_abap_source_id=>get_destination( srcid ) ) ).

    repository_proxy = COND #( WHEN srcid IS INITIAL THEN cl_sca_repository_proxy=>get_local_access( )
                                                     ELSE cl_sca_repository_proxy=>get_remote_access( i_rfc_destination = cl_abap_source_id=>get_destination( srcid ) ) ).
  ENDMETHOD.

  METHOD get_class_inheritance.
    DATA(sql) = NEW lcl_select( destination ).

    sql->add_column( 'refclsname' ).
    sql->set_table( 'seometarel' ).
    sql->add_where( |clsname = '{ clsname }'| ).
    sql->add_where( |refclsname IS NOT NULL| ).

    IF sql->run( ) = abap_false.
      RETURN.
    ENDIF.

    "result = sql->get_column( 'refclsname' ).
  ENDMETHOD.

  METHOD get_object_generated.
    DATA(sql) = NEW lcl_select( destination ).

    sql->add_column( 'GENFLAG' ).
    sql->set_table( 'TADIR' ).
    sql->add_where( |PGMID = 'R3TR'| ).
    sql->add_where( |AND OBJECT = '{ object_type }'| ).
    sql->add_where( |AND OBJ_NAME = '{ object_name }'| ).

    IF sql->run( ) = abap_false.
      RETURN.
    ENDIF.

    DATA tadir TYPE tadir.
    sql->get_result( IMPORTING result = tadir ).
    result = tadir-genflag.
  ENDMETHOD.

  METHOD is_business_object.
    DATA(sql) = NEW lcl_select( destination ).

    sql->add_column( 'NAME' ).
    sql->set_table( 'TOJTB' ).
    sql->add_where( |PROGNAME = '{ object_name }'| ).

    result = sql->run( ).
  ENDMETHOD.

  METHOD get_source_code.
    DATA(source) = NEW lcl_report_source( destination = destination
                                          object_type = object_type
                                          object_name = object_name ).

    source->run( ).

    result = source->get_source_code( ).
  ENDMETHOD.

  METHOD is_infotype.
    DATA(sql) = NEW lcl_select( destination ).

    sql->add_column( 'INFTY' ).
    sql->set_table( 'T777D' ).
    sql->add_where( |REPID = '{ object_name }'| ).
    sql->add_where( |OR BTCI_PROG = '{ object_name }'| ).

    result = sql->run( ).
  ENDMETHOD.

  METHOD get_trdir.
    DATA(sql) = NEW lcl_select( destination ).

    sql->set_table( 'trdir' ).
    sql->add_where( |name = '{ object_name }'| ).

    result = sql->run( ).
  ENDMETHOD.

ENDCLASS.

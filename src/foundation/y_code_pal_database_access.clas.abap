CLASS y_code_pal_database_access DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA repository_access TYPE REF TO if_sca_repository_access READ-ONLY.
    DATA repository_proxy  TYPE REF TO if_sca_repository_proxy READ-ONLY.

    METHODS constructor IMPORTING srcid TYPE scr_source_id.

    METHODS get_class_inheritance IMPORTING clsname       TYPE seometarel-clsname
                                  RETURNING VALUE(result) TYPE seometarel-refclsname.

    METHODS get_object_generated IMPORTING object_type   TYPE tadir-object
                                           object_name   TYPE tadir-obj_name
                                 RETURNING VALUE(result) TYPE tadir-genflag.

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
    result = lcl_select=>create( destination )->set_table( 'seometarel' )->add_column( 'refclsname' )->add_where( |clsname = '{ clsname }'| )->add_where( |refclsname IS NOT NULL| )->run( )->get_column( 'refclsname' ).
  ENDMETHOD.

  METHOD get_object_generated.
    DATA(sql) = lcl_select=>create( destination ).

    sql->set_table( 'TADIR' ).
    sql->add_column( 'GENFLAG' ).
    sql->add_where( |PGMID = 'R3TR'| ).
    sql->add_where( |AND OBJECT = '{ object_type }'| ).
    sql->add_where( |AND OBJ_NAME = '{ object_name }'| ).
    sql->run( ).

    result = sql->get_column( 'GENFLAG' ).
  ENDMETHOD.

ENDCLASS.

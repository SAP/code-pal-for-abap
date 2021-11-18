CLASS y_code_pal_database_access DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA repository_access TYPE REF TO if_sca_repository_access READ-ONLY.

    METHODS constructor IMPORTING srcid TYPE scr_source_id.

    METHODS get_class_inheritance IMPORTING clsname       TYPE seometarel-clsname
                                  RETURNING VALUE(result) TYPE seometarel-refclsname.

  PRIVATE SECTION.
    DATA destination TYPE rfcdest.

ENDCLASS.



CLASS y_code_pal_database_access IMPLEMENTATION.

  METHOD constructor.
    repository_access = COND #( WHEN srcid IS INITIAL THEN cl_sca_repository_access=>get_local_access( )
                                                      ELSE cl_sca_repository_access=>get_access_by_rfc( cl_abap_source_id=>get_destination( srcid ) ) ).
  ENDMETHOD.

  METHOD get_class_inheritance.
    result = lcl_select=>create( destination )->set_table( 'seometarel' )->add_column( 'refclsname' )->add_where( |clsname = '{ clsname }'| )->add_where( |refclsname IS NOT NULL| )->run( )->get_column( 'refclsname' ).
  ENDMETHOD.

ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_base IMPLEMENTATION.

  METHOD constructor.
    me->object_type = object_type.
    me->object_name = object_name.
    me->include = include.
    me->database_access  = database_access.
  ENDMETHOD.


  METHOD get_creation_date.
    DATA dates TYPE TABLE OF ty_created_on.

    dates = VALUE #( ( get_tadir_date( ) )
                     ( get_trdir_date( ) )
                     ( get_version_date( ) ) ).

    DELETE dates WHERE table_line IS INITIAL OR table_line = empty_date.

    IF lines( dates ) > 0.
      SORT dates ASCENDING.
      result = dates[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD get_tadir_date.
    DATA(keys) = VALUE if_sca_repository_type=>ty_object_keys( ( pgmid = 'R3TR'
                                                                 obj_type = object_type
                                                                 obj_name = object_name ) ).

    DATA(catalogues) = database_access->repository_access->get_object_catalogue_records( keys ).

    result = catalogues[ 1 ]-created_on.
  ENDMETHOD.


  METHOD get_trdir_date.
    DATA(trdir) = database_access->get_trdir( object_type = 'REPS'
                                              object_name = include ).
    IF lines( trdir ) > 0.
      result = trdir[ 1 ]-cdat.
    ENDIF.
  ENDMETHOD.


  METHOD get_version_date.
    DATA(vrsd) = database_access->get_version_management( object_type = get_version_access_type( )
                                                          object_name = get_version_access_name( ) ).

    DELETE vrsd WHERE datum IS INITIAL OR datum = empty_date.

    IF lines( vrsd ) > 0.
      SORT vrsd BY datum.
      result = vrsd[ 1 ]-datum.
    ENDIF.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_class IMPLEMENTATION.

  METHOD get_version_access_type.
    result = object_type.
  ENDMETHOD.

  METHOD get_version_access_name.
    result = |{ object_name ALIGN = LEFT WIDTH = 30 PAD = space }%|.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_function_group IMPLEMENTATION.

  METHOD get_version_access_type.
    result = 'REPS'.
  ENDMETHOD.

  METHOD get_version_access_name.
    result = include.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_program IMPLEMENTATION.

  METHOD get_version_access_type.
    result = 'REPS'.
  ENDMETHOD.

  METHOD get_version_access_name.
    result = object_name.
  ENDMETHOD.

ENDCLASS.




CLASS lcl_factory IMPLEMENTATION.

  METHOD create.
    CASE object_type.
      WHEN 'FUGR'.
        result = NEW lcl_function_group( object_type     = object_type
                                         object_name     = object_name
                                         include         = include
                                         database_access = database_access ).
      WHEN 'CLAS' OR 'INTF'.
        result = NEW lcl_class( object_type     = object_type
                                object_name     = object_name
                                include         = include
                                database_access = database_access ).
      WHEN 'REPS'.
        result = NEW lcl_program( object_type     = object_type
                                  object_name     = object_name
                                  include         = include
                                  database_access = database_access ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

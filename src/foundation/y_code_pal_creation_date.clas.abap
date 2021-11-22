CLASS y_code_pal_creation_date DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_creation_date.
    METHODS constructor IMPORTING database_access TYPE REF TO y_code_pal_database_access.

  PROTECTED SECTION.
    ALIASES database_access FOR y_if_code_pal_creation_date~database_access.
    ALIASES created_on_dates FOR y_if_code_pal_creation_date~created_on_dates.
    ALIASES buffer FOR y_if_code_pal_creation_date~buffer.
    ALIASES max_entries FOR y_if_code_pal_creation_date~max_entries.

  PRIVATE SECTION.
    METHODS get_db_tadir_data IMPORTING VALUE(object_type) TYPE trobjtype
                                        VALUE(object_name) TYPE sobj_name
                              RETURNING VALUE(result)      TYPE rdir_cdate.

    METHODS get_db_reposource_created_on IMPORTING VALUE(object_name) TYPE sobj_name
                                         RETURNING VALUE(result)      TYPE rdir_cdate.

    METHODS get_db_vers_hstry_crtd_on_clas IMPORTING VALUE(class_name) TYPE sobj_name
                                           RETURNING VALUE(result)     TYPE rdir_cdate.

    METHODS get_db_vers_hstry_crtd_on_prog IMPORTING VALUE(prog_name) TYPE sobj_name
                                           RETURNING VALUE(result)    TYPE rdir_cdate.

    METHODS get_db_vers_hstry_crtd_on_fugr IMPORTING VALUE(fugr_name) TYPE sobj_name
                                           RETURNING VALUE(result)    TYPE rdir_cdate.

    METHODS convert_fugr_for_db_access IMPORTING VALUE(name)   TYPE sobj_name
                                       RETURNING VALUE(result) TYPE string.

    METHODS convert_class_for_repos_access IMPORTING VALUE(name)   TYPE sobj_name
                                           RETURNING VALUE(result) TYPE string.

    METHODS get_lowest_date IMPORTING VALUE(dates)  TYPE created_on_dates
                            RETURNING VALUE(result) TYPE as4date.

    METHODS get_from_buffer IMPORTING VALUE(object_type) TYPE trobjtype
                                      VALUE(object_name) TYPE sobj_name
                            RETURNING VALUE(result)      TYPE rdir_cdate
                            RAISING   cx_sy_itab_line_not_found.

    METHODS try_new_created_on IMPORTING object_type   TYPE trobjtype
                                         object_name   TYPE sobj_name
                               RETURNING VALUE(result) TYPE as4date.

ENDCLASS.



CLASS y_code_pal_creation_date IMPLEMENTATION.

  METHOD constructor.
    me->database_access = database_access.
  ENDMETHOD.

  METHOD y_if_code_pal_creation_date~get_creation_date.
    TRY.
        result = get_from_buffer( object_type = object_type
                                  object_name =  object_name ).
      CATCH cx_sy_itab_line_not_found.
        result = try_new_created_on( object_type = object_type
                                     object_name =  object_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_from_buffer.
    result = buffer[ object_type = object_type
                     object_name = object_name ]-created_on.
  ENDMETHOD.


  METHOD try_new_created_on.
    DATA created_on_dates TYPE created_on_dates.

    IF lines( buffer ) > max_entries.
      DELETE buffer FROM 1 TO max_entries / 2.
    ENDIF.

    DATA(taidr) = get_db_tadir_data( object_type = object_type
                                     object_name = object_name ).

    APPEND taidr TO created_on_dates.

    DATA(repo_access) = COND #( WHEN object_type = 'FUGR' THEN convert_fugr_for_db_access( object_name )
                                WHEN object_type = 'CLAS' THEN convert_class_for_repos_access( object_name )
                                WHEN object_type = 'INTF' THEN convert_class_for_repos_access( object_name )
                                WHEN object_type = 'PROG' THEN object_name ).

    APPEND get_db_reposource_created_on( CONV #( repo_access ) ) TO created_on_dates.

    DATA(version_history) = COND #( WHEN object_type = 'FUGR' THEN get_db_vers_hstry_crtd_on_fugr( object_name )
                                    WHEN object_type = 'CLAS' THEN get_db_vers_hstry_crtd_on_clas( object_name )
                                    WHEN object_type = 'INTF' THEN get_db_vers_hstry_crtd_on_clas( object_name )
                                    WHEN object_type = 'PROG' THEN get_db_vers_hstry_crtd_on_prog( object_name ) ).

    APPEND version_history TO created_on_dates.

    result = get_lowest_date( created_on_dates  ).

    APPEND VALUE #( object_type = object_type
                    object_name = object_name
                    created_on = result ) TO buffer.
  ENDMETHOD.


  METHOD convert_class_for_repos_access.
    result = |{ name }%|.
  ENDMETHOD.


  METHOD convert_fugr_for_db_access.
    IF name(1) = '/'.
      SEARCH name FOR '/' STARTING AT 2.
      DATA(l_textcount) = sy-fdpos + 2.
      result = |{ name(l_textcount) }SAPL{ name+l_textcount }|.
    ELSE.
      result = |SAPL{ name }|.
    ENDIF.
  ENDMETHOD.


  METHOD get_db_reposource_created_on.
    DATA(creation_dates) = database_access->get_report_source( object_name ).

    SORT creation_dates BY cdat.
    DELETE creation_dates WHERE cdat IS INITIAL.

    TRY.
        result = creation_dates[ 1 ]-cdat.
      CATCH cx_sy_itab_line_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.


  METHOD get_db_tadir_data.
    DATA(keys) = VALUE if_sca_repository_type=>ty_object_keys( ( pgmid = 'R3TR'
                                                                 obj_type = object_type
                                                                 obj_name = object_name ) ).

    DATA(catalogues) = database_access->repository_access->get_object_catalogue_records( keys ).

    result = catalogues[ keys[ 1 ] ]-created_on.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_clas.
    DATA(class_search_string) = |{ class_name ALIGN = LEFT WIDTH = 30 PAD = space }%|.

    DATA(vrsd) = database_access->get_version_management( object_type = 'METH'
                                                          object_name = CONV #( class_search_string ) ).

    SORT vrsd BY datum.

    result = vrsd[ 1 ]-datum.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_fugr.
    DATA(search_pattern) = convert_fugr_for_db_access( fugr_name ).

    DATA(functions) = database_access->repository_access->get_functions_of_function_pool( CONV #( search_pattern ) ).

    result = sy-datum.

    LOOP AT functions ASSIGNING FIELD-SYMBOL(<function>).
      DATA(vrsd) = database_access->get_version_management( object_type = 'FUNC'
                                                            object_name = CONV #( <function>-funcname ) ).

      SORT vrsd BY datum.
      ASSIGN vrsd[ 1 ]-datum TO FIELD-SYMBOL(<datum>).

      IF <datum> < result.
        result = <datum>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_prog.
    DATA(vrsd) = database_access->get_version_management( object_type = 'REPS'
                                                          object_name = CONV #( prog_name ) ).

    SORT vrsd BY datum.
    result = vrsd[ 1 ]-datum.
  ENDMETHOD.


  METHOD get_lowest_date.
    DELETE dates WHERE table_line = '000000' OR table_line IS INITIAL.
    DESCRIBE TABLE dates LINES DATA(lines).
    IF lines > 0.
      SORT dates ASCENDING.
      READ TABLE dates INDEX 1 INTO result.
    ENDIF.
  ENDMETHOD.


ENDCLASS.

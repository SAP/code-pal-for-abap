CLASS y_object_creation_date DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get_created_on IMPORTING object_type   TYPE trobjtype
                                           object_name   TYPE sobj_name
                                 RETURNING VALUE(result) TYPE as4date.

  PRIVATE SECTION.
    TYPES: created_on_dates TYPE STANDARD TABLE OF as4date.

    TYPES: BEGIN OF ty_buffer,
             object_type TYPE trobjtype,
             object_name TYPE trobj_name,
             created_on  TYPE creationdt,
           END OF ty_buffer.

    TYPES: tty_buffer TYPE TABLE OF ty_buffer WITH KEY object_type
                                                       object_name.

    CONSTANTS max_entries TYPE i VALUE 100.

    CLASS-DATA buffer TYPE tty_buffer.

    CLASS-METHODS get_db_tadir_data IMPORTING VALUE(object_type) TYPE trobjtype
                                              VALUE(object_name) TYPE sobj_name
                                    RETURNING VALUE(result)      TYPE rdir_cdate.

    CLASS-METHODS get_db_reposource_created_on IMPORTING VALUE(reposrc_prog_search_string) TYPE string
                                               RETURNING VALUE(result)                     TYPE rdir_cdate.

    CLASS-METHODS get_db_vers_hstry_crtd_on_clas IMPORTING VALUE(class_name) TYPE sobj_name
                                                 RETURNING VALUE(result)     TYPE rdir_cdate.

    CLASS-METHODS get_db_vers_hstry_crtd_on_prog IMPORTING VALUE(prog_name) TYPE sobj_name
                                                 RETURNING VALUE(result)    TYPE rdir_cdate.

    CLASS-METHODS get_db_vers_hstry_crtd_on_fugr IMPORTING VALUE(fugr_name) TYPE sobj_name
                                                 RETURNING VALUE(result)    TYPE rdir_cdate.

    CLASS-METHODS convert_fugr_for_db_access IMPORTING VALUE(name)   TYPE sobj_name
                                             RETURNING VALUE(result) TYPE string.

    CLASS-METHODS convert_class_for_repos_access IMPORTING VALUE(name)   TYPE sobj_name
                                                 RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_lowest_date IMPORTING VALUE(dates)  TYPE created_on_dates
                                  RETURNING VALUE(result) TYPE as4date.

    CLASS-METHODS get_from_buffer IMPORTING VALUE(object_type) TYPE trobjtype
                                            VALUE(object_name) TYPE sobj_name
                                  RETURNING VALUE(result)      TYPE rdir_cdate
                                  RAISING   cx_sy_itab_line_not_found.

    CLASS-METHODS try_new_created_on IMPORTING object_type   TYPE trobjtype
                                               object_name   TYPE sobj_name
                                     RETURNING VALUE(result) TYPE as4date.

ENDCLASS.



CLASS y_object_creation_date IMPLEMENTATION.


  METHOD get_created_on.
    TRY.
        result = get_from_buffer( object_type = object_type
                                  object_name =  object_name ).
      CATCH cx_sy_itab_line_not_found.
        result = try_new_created_on( object_type = object_type
                                     object_name =  object_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_from_buffer.
    DATA(entry) = buffer[ object_type = object_type
                          object_name = object_name ].
    result = entry-created_on.
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

    APPEND get_db_reposource_created_on( repo_access ) TO created_on_dates.

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
    SELECT cdat
    FROM reposrc
    INTO TABLE @DATA(creation_dates)
    WHERE progname LIKE @reposrc_prog_search_string
    AND r3state = 'A'
    ORDER BY cdat.

    DELETE creation_dates WHERE cdat IS INITIAL.

    TRY.
        result = creation_dates[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.


  METHOD get_db_tadir_data.
    SELECT SINGLE created_on FROM tadir INTO @result WHERE
        pgmid = 'R3TR' AND
        object = @object_type AND
        obj_name = @object_name.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_clas.
    DATA(class_search_string) = |{ class_name ALIGN = LEFT WIDTH = 30 PAD = space }%|.

    SELECT SINGLE MIN( datum ) FROM vrsd WHERE
      objtype = 'METH' AND
      objname LIKE @class_search_string AND
      datum IS NOT NULL                                       "only in HRI
    INTO @DATA(earliest_transport).

    result = earliest_transport.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_fugr.
    DATA function_search_table TYPE RANGE OF sobj_name.
    DATA function_search LIKE LINE OF function_search_table.

    DATA(search_pattern) = convert_fugr_for_db_access( fugr_name ).

    SELECT funcname INTO TABLE @DATA(functions) FROM tfdir WHERE pname = @search_pattern. "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    function_search-sign = 'I'.
    function_search-option = 'EQ'.
    LOOP AT functions INTO DATA(l_function).
      function_search-low = l_function.
      APPEND function_search TO function_search_table.
    ENDLOOP.

    SELECT SINGLE MIN( datum ) FROM vrsd WHERE
      objtype = 'FUNC' AND
      objname IN @function_search_table AND
      datum IS NOT NULL AND                                     "only in HRI
      datum <> '00000000'
    INTO @result.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_prog.
    SELECT SINGLE MIN( datum ) FROM vrsd WHERE
      objtype = 'REPS' AND
      objname LIKE @prog_name AND
      datum IS NOT NULL                             "only in HRI
    INTO @result.
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

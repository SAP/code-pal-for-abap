CLASS y_object_creation_date DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES y_if_object_creation_date.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: created_on_dates TYPE STANDARD TABLE OF as4date .

    METHODS get_db_tadir_data
      IMPORTING
        VALUE(object_type) TYPE trobjtype
        VALUE(object_name) TYPE sobj_name
      RETURNING
        VALUE(result)      TYPE rdir_cdate .
    METHODS get_db_reposource_created_on
      IMPORTING
        VALUE(reposrc_prog_search_string) TYPE string
      RETURNING
        VALUE(result)                     TYPE rdir_cdate .
    METHODS get_db_vers_hstry_crtd_on_clas
      IMPORTING
        VALUE(class_name) TYPE sobj_name
      RETURNING
        VALUE(result)     TYPE rdir_cdate .
    METHODS get_db_vers_hstry_crtd_on_prog
      IMPORTING
        VALUE(prog_name) TYPE sobj_name
      RETURNING
        VALUE(result)    TYPE rdir_cdate .
    METHODS get_db_vers_hstry_crtd_on_fugr
      IMPORTING
        VALUE(fugr_name) TYPE sobj_name
      RETURNING
        VALUE(result)    TYPE rdir_cdate .
    METHODS convert_fugr_for_db_access
      IMPORTING
        VALUE(name)   TYPE sobj_name
      RETURNING
        VALUE(result) TYPE string .
    METHODS convert_class_for_repos_access
      IMPORTING
        VALUE(name)   TYPE sobj_name
      RETURNING
        VALUE(result) TYPE string .
    METHODS get_lowest_date
      IMPORTING
        VALUE(dates)  TYPE created_on_dates
      RETURNING
        VALUE(result) TYPE as4date .
    METHODS get_created_on_from_buffer
      IMPORTING
        VALUE(object_type) TYPE trobjtype
        VALUE(object_name) TYPE sobj_name
      RETURNING
        VALUE(result)      TYPE rdir_cdate
      RAISING
        cx_sy_itab_line_not_found .
    METHODS try_new_created_on
      IMPORTING
        object_type   TYPE trobjtype
        object_name   TYPE sobj_name
      RETURNING
        VALUE(result) TYPE as4date.
    METHODS get_created_on
      IMPORTING
        object_type   TYPE trobjtype
        object_name   TYPE sobj_name
      RETURNING
        VALUE(result) TYPE as4date.
ENDCLASS.



CLASS y_object_creation_date IMPLEMENTATION.


  METHOD convert_class_for_repos_access.
    result = name && '%'.
  ENDMETHOD.


  METHOD convert_fugr_for_db_access.
    IF name(1) = '/'.
      SEARCH name FOR '/' STARTING AT 2.
      DATA(l_textcount) = sy-fdpos + 2.                 "#EC DECL_IN_IF
      result = name(l_textcount) && 'SAPL' && name+l_textcount.
    ELSE.
      result = 'SAPL' && name.
    ENDIF.
  ENDMETHOD.


  METHOD get_created_on_from_buffer.
    result = y_code_pal_buffer=>get( object_type = object_type
                                     object_name = CONV #( object_name ) )-created_on.

    IF result IS INITIAL
    OR result = '000000'.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD get_db_reposource_created_on.
    SELECT SINGLE MIN( cdat ) AS creation_date  FROM reposrc INTO @result WHERE
        progname LIKE @reposrc_prog_search_string AND
        r3state = 'A' AND
        cdat > '00000000'.
  ENDMETHOD.


  METHOD get_db_tadir_data.
    TRY.
        DATA(entry) = y_code_pal_tadir_da=>get( program_id = 'R3TR'
                                                object_type = object_type
                                                object_name = object_name ).
        result = entry-created_on.
      CATCH ycx_entry_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_clas.
    DATA class_search_string TYPE string.

    class_search_string = class_name.
    WHILE strlen( class_search_string ) < 30.
      CONCATENATE class_search_string ` ` INTO class_search_string.
    ENDWHILE.
    class_search_string = class_search_string && '%'.

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
    IF sy-subrc NE 0.
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
      datum NE '00000000'
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


  METHOD y_if_object_creation_date~get_class_create_date.
    result = get_created_on( object_type = 'CLAS'
                             object_name = name ).
  ENDMETHOD.


  METHOD y_if_object_creation_date~get_function_group_create_date.
    result = get_created_on( object_type = 'FUGR'
                             object_name = name ).
  ENDMETHOD.


  METHOD y_if_object_creation_date~get_interface_create_date.
    result = get_created_on( object_type = 'INTF'
                             object_name = name ).
  ENDMETHOD.


  METHOD y_if_object_creation_date~get_program_create_date.
    result = get_created_on( object_type = 'PROG'
                             object_name = name ).
  ENDMETHOD.

  METHOD get_created_on.
    TRY.
        result = get_created_on_from_buffer( object_type  = object_type
                                             object_name  =  object_name ).
      CATCH cx_sy_itab_line_not_found.
        result = try_new_created_on( object_type  = object_type
                                     object_name  =  object_name ).
    ENDTRY.
  ENDMETHOD.

  METHOD try_new_created_on.

    DATA created_on_dates TYPE created_on_dates.

    APPEND get_db_tadir_data( object_type = object_type object_name = object_name ) TO created_on_dates.

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

    y_code_pal_buffer=>modify( VALUE #( object_type = object_type
                                        object_name = object_name
                                        created_on = result ) ).

  ENDMETHOD.


ENDCLASS.

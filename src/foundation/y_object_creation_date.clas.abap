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
        VALUE(object_type)   TYPE trobjtype
        VALUE(object_name)   TYPE sobj_name
      RETURNING
        VALUE(result) TYPE rdir_cdate .
    METHODS get_db_reposource_created_on
      IMPORTING
        VALUE(reposrc_prog_search_string) TYPE string
      RETURNING
        VALUE(result)              TYPE rdir_cdate .
    METHODS get_db_vers_hstry_crtd_on_clas
      IMPORTING
        VALUE(class_name)    TYPE sobj_name
      RETURNING
        VALUE(result) TYPE rdir_cdate .
    METHODS get_db_vers_hstry_crtd_on_prog
      IMPORTING
        VALUE(prog_name)     TYPE sobj_name
      RETURNING
        VALUE(result) TYPE rdir_cdate .
    METHODS get_db_vers_hstry_crtd_on_fugr
      IMPORTING
        VALUE(fugr_name)     TYPE sobj_name
      RETURNING
        VALUE(result) TYPE rdir_cdate .
    METHODS convert_fugr_for_db_access
      IMPORTING
        VALUE(name)              TYPE sobj_name
      RETURNING
        VALUE(result) TYPE string .
    METHODS convert_class_for_repos_access
      IMPORTING
        VALUE(name)              TYPE sobj_name
      RETURNING
        VALUE(result) TYPE string .
    METHODS get_lowest_date
      IMPORTING
        VALUE(dates)      TYPE created_on_dates
      RETURNING
        VALUE(result) TYPE as4date .
    METHODS get_created_on_from_buffer
      IMPORTING
        VALUE(object_type)   TYPE trobjtype
        VALUE(object_name)   TYPE sobj_name
      EXPORTING
        VALUE(creation_date) TYPE rdir_cdate
        VALUE(is_in_buffer)  TYPE abap_bool .
    METHODS insert_created_on_to_buffer
      IMPORTING
        VALUE(object_type)   TYPE trobjtype
        VALUE(object_name)   TYPE sobj_name
        VALUE(creation_date) TYPE rdir_cdate .
ENDCLASS.



CLASS Y_OBJECT_CREATION_DATE IMPLEMENTATION.


  METHOD convert_class_for_repos_access.
    result = name && '%'.
  ENDMETHOD.


  METHOD convert_fugr_for_db_access.
    IF name(1) = '/'.
      SEARCH name FOR '/' STARTING AT 2.
      DATA(l_textcount) = sy-fdpos + 2. "#EC DECL_IN_IF
      result = name(l_textcount) && 'SAPL' && name+l_textcount.
    ELSE.
      result = 'SAPL' && name.
    ENDIF.
  ENDMETHOD.


  METHOD get_created_on_from_buffer.
    SELECT SINGLE created_on FROM ytab_exemptions INTO @creation_date
      WHERE object                 = @object_type AND
            obj_name               = @object_name AND
            is_created_on_buffered = @abap_true.
    IF sy-subrc = 0 AND creation_date IS NOT INITIAL AND creation_date <> '000000'.
      is_in_buffer = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_db_reposource_created_on.
    SELECT SINGLE MIN( cdat ) AS creation_date  FROM reposrc INTO @result WHERE
        progname LIKE @reposrc_prog_search_string AND
        r3state = 'A' AND
        cdat > '00000000'.
  ENDMETHOD.


  METHOD get_db_tadir_data.
    SELECT SINGLE created_on FROM tadir INTO @result WHERE
        pgmid = 'R3TR' AND
        object = @object_type AND
        obj_name = @object_name.
  ENDMETHOD.


  METHOD get_db_vers_hstry_crtd_on_clas.
    DATA: class_search_string TYPE string.

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


  METHOD insert_created_on_to_buffer.
    DATA: exemption TYPE ytab_exemptions.

    exemption-object = object_type.
    exemption-obj_name = object_name.
    exemption-created_on = creation_date.
    exemption-as4date_co = sy-datum.
    exemption-is_created_on_buffered = abap_true.

    INSERT ytab_exemptions FROM exemption.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    UPDATE ytab_exemptions SET created_on = @creation_date,
                               as4date_co = @sy-datum,
                               is_created_on_buffered = @abap_true
                           WHERE object = @object_type
                           AND obj_name = @object_name.

    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD y_if_object_creation_date~get_class_create_date.
    DATA: created_on_dates TYPE created_on_dates.

    get_created_on_from_buffer(
         EXPORTING
           object_type  = 'CLAS'
           object_name  =  name
         IMPORTING
           creation_date = createdate
           is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.

    DATA(tadir_date) = get_db_tadir_data( object_type = 'CLAS' object_name = name ).
    APPEND tadir_date TO created_on_dates.
    DATA(reosource_key) = convert_class_for_repos_access( name ).
    DATA(repo_date) = get_db_reposource_created_on( reposrc_prog_search_string = reosource_key ).
    APPEND repo_date TO created_on_dates.
    DATA(vers_date) = get_db_vers_hstry_crtd_on_clas( name ).
    APPEND vers_date TO created_on_dates.

    createdate = get_lowest_date( created_on_dates  ).

    insert_created_on_to_buffer( object_type   = 'CLAS'
                                 object_name   = name
                                 creation_date = createdate ).
  ENDMETHOD.


  METHOD y_if_object_creation_date~get_function_group_create_date.
    DATA: created_on_dates TYPE created_on_dates.

    get_created_on_from_buffer(
     EXPORTING
       object_type  = 'FUGR'
       object_name  =  name
     IMPORTING
       creation_date = createdate
       is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.


    DATA(tadir_date) = get_db_tadir_data( object_type = 'FUGR' object_name = name ).
    APPEND tadir_date TO created_on_dates.
    DATA(reosource_key) = convert_fugr_for_db_access( name ).
    DATA(repo_date) = get_db_reposource_created_on( reposrc_prog_search_string = reosource_key ).
    APPEND repo_date TO created_on_dates.
    DATA(vers_date) = get_db_vers_hstry_crtd_on_fugr( name ).
    APPEND vers_date TO created_on_dates.

    createdate = get_lowest_date( created_on_dates  ).

    insert_created_on_to_buffer( object_type   = 'FUGR'
                                 object_name   = name
                                 creation_date = createdate ).
  ENDMETHOD.


  METHOD y_if_object_creation_date~get_interface_create_date.
    DATA: created_on_dates TYPE created_on_dates.

    get_created_on_from_buffer(
         EXPORTING
           object_type  = 'INTF'
           object_name  =  name
         IMPORTING
           creation_date = createdate
           is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.

    DATA(tadir_date) = get_db_tadir_data( object_type = 'INTF' object_name = name ).
    APPEND tadir_date TO created_on_dates.
    DATA(reosource_key) = convert_class_for_repos_access( name ).
    DATA(repo_date) = get_db_reposource_created_on( reposrc_prog_search_string = reosource_key ).
    APPEND repo_date TO created_on_dates.
    DATA(vers_date) = get_db_vers_hstry_crtd_on_clas( name ).
    APPEND vers_date TO created_on_dates.

    createdate = get_lowest_date( created_on_dates  ).

   insert_created_on_to_buffer( object_type   = 'INTF'
                                object_name   = name
                                creation_date = createdate ).
  ENDMETHOD.


  METHOD y_if_object_creation_date~get_program_create_date.
    DATA: created_on_dates TYPE created_on_dates,
          helper           TYPE string.

    helper = name.

    get_created_on_from_buffer(
         EXPORTING
           object_type  = 'PROG'
           object_name  =  name
         IMPORTING
           creation_date = createdate
           is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.

    DATA(repo_date) = get_db_reposource_created_on( helper ).
    APPEND repo_date TO created_on_dates.
    DATA(tadir_date) = get_db_tadir_data( object_type = 'PROG' object_name = name ).
    APPEND tadir_date TO created_on_dates.
    DATA(vers_date) = get_db_vers_hstry_crtd_on_prog( name ).
    APPEND vers_date TO created_on_dates.

    createdate = get_lowest_date( created_on_dates  ).

    insert_created_on_to_buffer( object_type   = 'PROG'
                                 object_name   = name
                                 creation_date = createdate ).
  ENDMETHOD.
ENDCLASS.

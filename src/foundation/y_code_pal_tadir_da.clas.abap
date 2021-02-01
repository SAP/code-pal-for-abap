CLASS y_code_pal_tadir_da DEFINITION SHARED MEMORY ENABLED PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get IMPORTING program_id TYPE tadir-pgmid
                                object_type TYPE tadir-object
                                object_name TYPE tadir-obj_name
                      RETURNING VALUE(result) TYPE tadir
                      RAISING ycx_entry_not_found.

  PRIVATE SECTION.
    CONSTANTS max_entries TYPE i VALUE 100.

    CLASS-DATA buffer TYPE TABLE OF tadir.

    CLASS-METHODS new IMPORTING program_id TYPE tadir-pgmid
                                object_type TYPE tadir-object
                                object_name TYPE tadir-obj_name
                       RETURNING VALUE(result) TYPE tadir
                       RAISING ycx_entry_not_found.

ENDCLASS.


CLASS y_code_pal_tadir_da IMPLEMENTATION.


  METHOD get.
    TRY.
        result = buffer[ pgmid = program_id
                         object = object_type
                         obj_name = object_name ].
      CATCH cx_sy_itab_line_not_found.
        result = new( program_id = program_id
                      object_type = object_type
                      object_name = object_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD new.
    IF lines( buffer ) > max_entries.
      CLEAR buffer.
    ENDIF.

    SELECT SINGLE *
    FROM tadir
    INTO @result
    WHERE pgmid = @program_id
    AND object = @object_type
    AND obj_name = @object_name.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.

    APPEND result TO buffer.
  ENDMETHOD.


ENDCLASS.

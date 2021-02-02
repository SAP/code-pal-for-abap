CLASS y_code_pal_app_comp DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES: BEGIN OF entry,
             level TYPE slevel,
             application_component TYPE tdevc-component,
           END OF entry.

    TYPES entries TYPE TABLE OF entry.

    CLASS-METHODS get IMPORTING level         TYPE entry-level
                      RETURNING VALUE(result) TYPE entry-application_component
                      RAISING ycx_entry_not_found.

  PRIVATE SECTION.
    CONSTANTS max_entries TYPE i VALUE 50.

    CLASS-DATA buffer TYPE entries.

    CLASS-METHODS new IMPORTING level         TYPE entry-level
                      RETURNING VALUE(result) TYPE entry
                      RAISING ycx_entry_not_found.

ENDCLASS.



CLASS y_code_pal_app_comp IMPLEMENTATION.


  METHOD get.
    TRY.
        result = buffer[ level = level ]-application_component.
      CATCH cx_sy_itab_line_not_found.
        result = new( level )-application_component.
    ENDTRY.
  ENDMETHOD.


  METHOD new.
    DATA tadir TYPE tadir.

    IF lines( buffer ) > max_entries.
      DELETE buffer FROM 1 TO max_entries / 2.
    ENDIF.

    CALL FUNCTION 'TR_TRANSFORM_TRDIR_TO_TADIR'
      EXPORTING
        iv_trdir_name = level-name
      IMPORTING
        es_tadir_keys = tadir.

    SELECT SINGLE td~component
    FROM tadir AS ta
    INNER JOIN tdevc AS td ON ta~devclass = td~devclass
    INTO @DATA(application_component)
    WHERE pgmid = @tadir-pgmid
    AND object = @tadir-object
    AND obj_name = @tadir-obj_name.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.

    result = VALUE #( level = level
                      application_component = application_component ).

    APPEND result TO buffer.
  ENDMETHOD.


ENDCLASS.

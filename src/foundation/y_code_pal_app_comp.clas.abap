CLASS y_code_pal_app_comp DEFINITION SHARED MEMORY ENABLED PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES: BEGIN OF entry,
             program               TYPE tadir-obj_name,
             application_component TYPE df14l-ps_posid,
           END OF entry.

    TYPES entries TYPE TABLE OF entry.

    CLASS-METHODS get IMPORTING program       TYPE entry-program
                      RETURNING VALUE(result) TYPE entry-application_component
                      RAISING ycx_entry_not_found.

  PRIVATE SECTION.
    CONSTANTS max_entries TYPE i VALUE 50.

    CLASS-DATA buffer TYPE entries.

    CLASS-METHODS get_entry IMPORTING program       TYPE entry-program
                            RETURNING VALUE(result) TYPE entry
                            RAISING ycx_entry_not_found.

    CLASS-METHODS new_entry IMPORTING program       TYPE entry-program
                            RETURNING VALUE(result) TYPE entry
                            RAISING ycx_entry_not_found.

ENDCLASS.



CLASS y_code_pal_app_comp IMPLEMENTATION.


  METHOD get.
    DATA(entry) = get_entry( program ).
    result = entry-application_component.
  ENDMETHOD.


  METHOD get_entry.
    TRY.
        result = buffer[ program = program ].
      CATCH cx_sy_itab_line_not_found.
        result = new_entry( program ).
    ENDTRY.
  ENDMETHOD.


  METHOD new_entry.
    IF lines( buffer ) > max_entries.
      DELETE buffer FROM 1 TO max_entries / 2.
    ENDIF.

    SELECT SINGLE ta~obj_name, df~ps_posid
    FROM tadir AS ta
    LEFT JOIN tdevc AS td ON ta~devclass = td~devclass
    LEFT JOIN df14l AS df ON td~component = df~fctr_id
    INTO @result
    WHERE ta~pgmid = 'R3TR'
    AND ta~object = 'PROG'
    AND ta~obj_name = @program.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.

    APPEND result TO buffer.
  ENDMETHOD.


ENDCLASS.

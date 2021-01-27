CLASS y_code_pal_app_comp DEFINITION SHARED MEMORY ENABLED PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES: BEGIN OF entry,
             pgmid TYPE tadir-pgmid,
             object TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             application_component TYPE df14l-ps_posid,
           END OF entry.

    TYPES entries TYPE TABLE OF entry.

    CLASS-METHODS get IMPORTING tadir         TYPE tadir
                      RETURNING VALUE(result) TYPE entry-application_component
                      RAISING ycx_entry_not_found.

  PRIVATE SECTION.
    CONSTANTS max_entries TYPE i VALUE 50.

    CLASS-DATA buffer TYPE entries.

    CLASS-METHODS get_entry IMPORTING tadir         TYPE tadir
                            RETURNING VALUE(result) TYPE entry
                            RAISING ycx_entry_not_found.

    CLASS-METHODS new_entry IMPORTING tadir         TYPE tadir
                            RETURNING VALUE(result) TYPE entry
                            RAISING ycx_entry_not_found.

ENDCLASS.



CLASS y_code_pal_app_comp IMPLEMENTATION.


  METHOD get.
    DATA(entry) = get_entry( tadir ).
    result = entry-application_component.
  ENDMETHOD.


  METHOD get_entry.
    TRY.
        result = buffer[ pgmid = tadir-pgmid
                         object = tadir-object
                         obj_name = tadir-obj_name  ].
      CATCH cx_sy_itab_line_not_found.
        result = new_entry( tadir ).
    ENDTRY.
  ENDMETHOD.


  METHOD new_entry.
    IF lines( buffer ) > 50.
      CLEAR buffer.
    ENDIF.

    SELECT SINGLE df~ps_posid
    FROM tdevc AS td
    LEFT JOIN df14l AS df ON td~component = df~fctr_id
    INTO @DATA(application_component)
    WHERE td~devclass = @tadir-devclass.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.

    result = VALUE #( pgmid = tadir-pgmid
                      object = tadir-object
                      obj_name = tadir-obj_name
                      application_component = application_component ).

    APPEND result TO buffer.
  ENDMETHOD.


ENDCLASS.

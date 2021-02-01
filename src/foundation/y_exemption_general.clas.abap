CLASS y_exemption_general DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_exemption.
    ALIASES create FOR y_if_exemption~create.

  PRIVATE SECTION.
    METHODS is_tadir_generated IMPORTING object_type   TYPE tadir-object
                                         object_name   TYPE tadir-obj_name
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_object_existing IMPORTING object_type   TYPE tadir-object
                                         object_name   TYPE tadir-obj_name
                               RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS y_exemption_general IMPLEMENTATION.

  METHOD y_if_exemption~create.
    result = NEW y_exemption_general( ).
  ENDMETHOD.

  METHOD y_if_exemption~is_object_exempted.
    result = xsdbool( ( is_object_existing( object_type = object_type object_name = object_name ) = abap_true ) OR
                      ( is_tadir_generated( object_type = object_type object_name = object_name ) = abap_true ) ).
  ENDMETHOD.


  METHOD is_object_existing.
    CONSTANTS object_exists TYPE char1 VALUE 'X'.

    DATA existence_flag TYPE strl_pari-flag.
    DATA l_object_type  TYPE e071-object.
    DATA l_object_name  TYPE e071-obj_name.

    l_object_type = object_type.
    l_object_name = object_name.

    CALL FUNCTION 'TR_CHECK_EXIST'
      EXPORTING
        iv_pgmid             = 'R3TR'
        iv_object            = l_object_type
        iv_obj_name          = l_object_name
      IMPORTING
        e_exist              = existence_flag
      EXCEPTIONS
        tr_no_check_function = 1.

    IF sy-subrc = 0 AND existence_flag <> object_exists.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_tadir_generated.
    TRY.
        DATA(entry) = y_code_pal_tadir_da=>get( program_id = 'R3TR'
                                                object_type = object_type
                                                object_name = object_name ).
        result = entry-genflag.
      CATCH ycx_entry_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.


ENDCLASS.

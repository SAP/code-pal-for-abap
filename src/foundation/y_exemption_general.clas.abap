CLASS y_exemption_general DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_exemption.
    ALIASES is_object_exempted FOR y_if_exemption~is_object_exempted.

  PRIVATE SECTION.
    CLASS-METHODS is_tadir_generated IMPORTING object_type   TYPE trobjtype
                                               object_name   TYPE sobj_name
                                     RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_object_existing IMPORTING object_type   TYPE trobjtype
                                               object_name   TYPE sobj_name
                                     RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS y_exemption_general IMPLEMENTATION.


  METHOD y_if_exemption~is_object_exempted.
    DATA(object_exists) = is_object_existing( object_type = object_type
                                              object_name = object_name ).

    DATA(tadir_generated) = is_tadir_generated( object_type = object_type
                                                object_name = object_name ).

    result = xsdbool( object_exists = abap_true OR tadir_generated = abap_true ).
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
    SELECT SINGLE genflag
    FROM tadir
    INTO @result
    WHERE pgmid = 'R3TR'
    AND object = @object_type
    AND obj_name = @object_name.
  ENDMETHOD.


ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_include_to_object_keys IMPLEMENTATION.

  METHOD convert.
    DATA tadir TYPE tadir.

    CALL FUNCTION 'TR_TRANSFORM_TRDIR_TO_TADIR'
      EXPORTING
        iv_trdir_name = include
      IMPORTING
        es_tadir_keys = tadir.

    result = CORRESPONDING #( tadir MAPPING obj_type = object ).
  ENDMETHOD.

ENDCLASS.

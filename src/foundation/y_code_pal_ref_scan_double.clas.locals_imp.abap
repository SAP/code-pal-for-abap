*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS fake_tadir DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get RETURNING VALUE(result) TYPE trdir.
  PROTECTED SECTION.
    CLASS-METHODS read.
  PRIVATE SECTION.
    CLASS-DATA trdir TYPE trdir.

ENDCLASS.



CLASS fake_tadir IMPLEMENTATION.

  METHOD get.
    IF trdir IS INITIAL.
      read( ).
    ENDIF.
    result = trdir.
  ENDMETHOD.

  METHOD read.
    SELECT SINGLE *
    FROM trdir
    INTO @trdir
    WHERE name = @sy-repid.
  ENDMETHOD.

ENDCLASS.

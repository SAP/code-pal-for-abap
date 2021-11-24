*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES database_access TYPE REF TO y_code_pal_database_access.
TYPES object_type TYPE trobjtype.
TYPES object_name TYPE sobj_name.
TYPES include TYPE program.

CLASS lcl_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES ty_created_on TYPE tadir-created_on.

    CONSTANTS empty_date TYPE ty_created_on VALUE '00000000'.

    DATA database_access TYPE database_access READ-ONLY.
    DATA object_type     TYPE object_type READ-ONLY.
    DATA object_name     TYPE object_name READ-ONLY.
    DATA include         TYPE include READ-ONLY.

    METHODS constructor IMPORTING object_type     TYPE object_type
                                  object_name     TYPE object_name
                                  include         TYPE include
                                  database_access TYPE database_access.

    METHODS get_creation_date RETURNING VALUE(result) TYPE ty_created_on.

  PROTECTED SECTION.
    METHODS get_version_access_type ABSTRACT RETURNING VALUE(result) TYPE versobjtyp.
    METHODS get_version_access_name ABSTRACT RETURNING VALUE(result) TYPE versobjnam.

  PRIVATE SECTION.
    METHODS get_tadir_date RETURNING VALUE(result) TYPE ty_created_on.
    METHODS get_trdir_date RETURNING VALUE(result) TYPE ty_created_on.
    METHODS get_version_date RETURNING VALUE(result) TYPE ty_created_on.

ENDCLASS.



CLASS lcl_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING object_type     TYPE object_type
                                   object_name     TYPE object_name
                                   include         TYPE include
                                   database_access TYPE database_access
                         RETURNING VALUE(result)   TYPE REF TO lcl_base.
ENDCLASS.



CLASS lcl_function_group DEFINITION INHERITING FROM lcl_base.
  PROTECTED SECTION.
    METHODS get_version_access_type REDEFINITION.
    METHODS get_version_access_name REDEFINITION.

ENDCLASS.



CLASS lcl_class DEFINITION INHERITING FROM lcl_base.
  PROTECTED SECTION.
    METHODS get_version_access_type REDEFINITION.
    METHODS get_version_access_name REDEFINITION.

ENDCLASS.



CLASS lcl_program DEFINITION INHERITING FROM lcl_base.
  PROTECTED SECTION.
    METHODS get_version_access_type REDEFINITION.
    METHODS get_version_access_name REDEFINITION.

ENDCLASS.

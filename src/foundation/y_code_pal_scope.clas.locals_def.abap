*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_include_to_object_keys DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert IMPORTING include TYPE progname
                          RETURNING VALUE(result) TYPE if_sca_repository_type=>ty_object_key.

ENDCLASS.

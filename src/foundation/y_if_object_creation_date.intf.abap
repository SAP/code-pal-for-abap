INTERFACE y_if_object_creation_date PUBLIC.

  METHODS get_class_create_date IMPORTING VALUE(name)   TYPE sobj_name
                                RETURNING VALUE(result) TYPE as4date.

  METHODS get_function_group_create_date IMPORTING VALUE(name)   TYPE sobj_name
                                         RETURNING VALUE(result) TYPE as4date.

  METHODS get_interface_create_date  IMPORTING name          TYPE sobj_name
                                     RETURNING VALUE(result) TYPE as4date.

  METHODS get_program_create_date IMPORTING VALUE(name)   TYPE sobj_name
                                  RETURNING VALUE(result) TYPE as4date.

ENDINTERFACE.

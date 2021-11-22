INTERFACE y_if_code_pal_creation_date PUBLIC.

  TYPES created_on_dates TYPE STANDARD TABLE OF as4date.

  TYPES: BEGIN OF ty_buffer,
           object_type TYPE trobjtype,
           object_name TYPE trobj_name,
           created_on  TYPE creationdt,
         END OF ty_buffer.

  TYPES tty_buffer TYPE TABLE OF ty_buffer WITH KEY object_type
                                                    object_name.

  CONSTANTS max_entries TYPE i VALUE 100.

  CLASS-DATA buffer TYPE tty_buffer READ-ONLY.

  DATA database_access TYPE REF TO y_code_pal_database_access READ-ONLY.

  METHODS get_creation_date IMPORTING object_type   TYPE trobjtype
                                      object_name   TYPE sobj_name
                            RETURNING VALUE(result) TYPE as4date.

ENDINTERFACE.

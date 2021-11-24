INTERFACE y_if_code_pal_exemption PUBLIC.

  TYPES: BEGIN OF ty_buffer,
           object_type TYPE trobjtype,
           object_name TYPE trobj_name,
           include TYPE program,
           is_exempted TYPE abap_bool,
         END OF ty_buffer.

  TYPES tty_buffer TYPE TABLE OF ty_buffer WITH KEY object_type
                                                    object_name
                                                    include.

  CONSTANTS max_entries TYPE i VALUE 100.

  DATA buffer TYPE tty_buffer READ-ONLY.

  DATA database_access TYPE REF TO y_if_code_pal_database_access READ-ONLY.

  METHODS is_exempt IMPORTING object_type   TYPE trobjtype
                              object_name   TYPE sobj_name
                              include       TYPE program
                    RETURNING VALUE(result) TYPE abap_bool.

ENDINTERFACE.

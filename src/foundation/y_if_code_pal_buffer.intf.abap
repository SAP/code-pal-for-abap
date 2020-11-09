INTERFACE y_if_code_pal_buffer PUBLIC.

  TYPES: BEGIN OF entry,
           object_type TYPE trobjtype,
           object_name TYPE trobj_name,
           is_exempted TYPE abap_bool,
           created_on TYPE creationdt,
         END OF entry.

  TYPES: entries TYPE TABLE OF entry WITH KEY object_type
                                              object_name.

  CLASS-METHODS get IMPORTING object_type   TYPE entry-object_type
                              object_name   TYPE entry-object_name
                    RETURNING VALUE(result) TYPE entry
                    RAISING cx_sy_itab_line_not_found.

  CLASS-METHODS modify IMPORTING entry TYPE entry.

ENDINTERFACE.

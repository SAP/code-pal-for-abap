INTERFACE y_if_code_pal_scope PUBLIC.

  TYPES: BEGIN OF ty_buffer,
           include               TYPE program,
           application_component TYPE tdevc-component,
         END OF ty_buffer.

  TYPES tty_buffer TYPE TABLE OF ty_buffer WITH KEY include.

  CONSTANTS max_entries TYPE i VALUE 100.

  CLASS-DATA buffer TYPE tty_buffer READ-ONLY.

  DATA database_access TYPE REF TO y_if_code_pal_database_access READ-ONLY.
  DATA leading_application_component TYPE ty_buffer-application_component READ-ONLY.

  METHODS is_it_in_scope IMPORTING include        TYPE program
                          RETURNING VALUE(result) TYPE abap_bool.

ENDINTERFACE.

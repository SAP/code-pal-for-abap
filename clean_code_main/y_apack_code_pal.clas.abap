CLASS y_apack_code_pal DEFINITION PUBLIC FINAL CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_apack_manifest.
    ALIASES descriptor FOR if_apack_manifest~descriptor.
    METHODS constructor.
ENDCLASS.

CLASS y_apack_code_pal IMPLEMENTATION.

  METHOD constructor.
    descriptor-group_id = 'github.com/SAP'.
    descriptor-artifact_id = 'code-pal-for-abap'.
    descriptor-version = '1.0.0'.
    descriptor-repository_type  = 'abapGit'.
    descriptor-git_url = 'https://github.com/SAP/code-pal-for-abap'.
  ENDMETHOD.

ENDCLASS.

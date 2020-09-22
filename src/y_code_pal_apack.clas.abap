CLASS y_code_pal_apack DEFINITION PUBLIC FINAL CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_apack_manifest.
    ALIASES descriptor FOR zif_apack_manifest~descriptor.
    METHODS constructor.
ENDCLASS.

CLASS y_code_pal_apack IMPLEMENTATION.

  METHOD constructor.
    descriptor-group_id = 'github.com/SAP'.
    descriptor-artifact_id = 'code-pal-for-abap'.
    descriptor-version = y_code_pal_version=>abap.
    descriptor-repository_type  = 'abapGit'.
    descriptor-git_url = 'https://github.com/SAP/code-pal-for-abap'.
  ENDMETHOD.

ENDCLASS.

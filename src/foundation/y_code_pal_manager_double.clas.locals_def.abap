*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS ltd_exemption DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_exemption.

ENDCLASS.



CLASS ltd_creation_date DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_creation_date.
    METHODS constructor IMPORTING check TYPE REF TO y_code_pal_base.

  PRIVATE SECTION.
    DATA check TYPE REF TO y_code_pal_base.

ENDCLASS.



CLASS ltd_statistics DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_statistics.

  PROTECTED SECTION.
    ALIASES count FOR y_if_code_pal_statistics~count.

  PRIVATE SECTION.
    CONSTANTS ec_prefix  TYPE string VALUE `#EC`.
    CONSTANTS ec_comment TYPE string VALUE `"#EC`.

    DATA pcom     TYPE sci_pcom.
    DATA pcom_alt TYPE sci_pcom.

    METHODS is_pseudo_comment IMPORTING ref_scan      TYPE REF TO cl_ci_scan
                                        scimessages   TYPE scimessages
                                        test          TYPE sci_chk
                                        code          TYPE sci_errc
                                        suppress      TYPE sci_pcom
                                        position      TYPE int4
                              RETURNING VALUE(result) TYPE sychar01.

    METHODS determine_pseudo_comments IMPORTING scimessages TYPE scimessages
                                                test        TYPE sci_chk
                                                code        TYPE sci_errc
                                                suppress    TYPE sci_pcom.

    METHODS has_comment IMPORTING ref_scan TYPE REF TO cl_ci_scan
                                  position TYPE int4
                        RETURNING VALUE(result) TYPE sci_pcom.

    METHODS has_inline_comment IMPORTING ref_scan      TYPE REF TO cl_ci_scan
                                         position      TYPE int4
                               RETURNING VALUE(result) TYPE sci_pcom.
ENDCLASS.



CLASS ltd_scope DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_scope.

ENDCLASS.



CLASS ltd_profile DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_profile.

ENDCLASS.

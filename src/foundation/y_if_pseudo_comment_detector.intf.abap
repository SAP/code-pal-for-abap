INTERFACE y_if_pseudo_comment_detector PUBLIC.
  CONSTANTS:
    ec_prefix  TYPE string VALUE `#EC`,
    ec_comment TYPE string VALUE `"#EC`.

  METHODS:
    is_pseudo_comment
      IMPORTING
                !ref_scan_manager TYPE REF TO y_if_scan_manager
                !scimessages      TYPE scimessages
                !test             TYPE sci_chk
                !code             TYPE sci_errc
                !suppress         TYPE sci_pcom OPTIONAL
                !position         TYPE int4
      RETURNING VALUE(result)     TYPE sychar01.
ENDINTERFACE.

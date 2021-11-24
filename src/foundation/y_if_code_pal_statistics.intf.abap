INTERFACE y_if_code_pal_statistics PUBLIC.

  DATA: BEGIN OF count READ-ONLY,
          errors TYPE i,
          warnings TYPE i,
          notes TYPE i,
          pseudo_comments TYPE i,
        END OF count.

  METHODS collect IMPORTING ref_scan         TYPE REF TO cl_ci_scan
                            scimessages      TYPE scimessages
                            test             TYPE sci_chk
                            code             TYPE sci_errc
                            kind             TYPE sychar01
                            suppress         TYPE sci_pcom
                            position         TYPE int4.

ENDINTERFACE.

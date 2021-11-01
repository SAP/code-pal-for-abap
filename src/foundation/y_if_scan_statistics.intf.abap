INTERFACE y_if_scan_statistics PUBLIC.

  DATA: BEGIN OF count READ-ONLY,
          errors TYPE i,
          warnings TYPE i,
          notes TYPE i,
          pseudo_comments TYPE i,
        END OF count.

  METHODS collect IMPORTING kind TYPE sychar01
                            pc   TYPE sychar01.

ENDINTERFACE.

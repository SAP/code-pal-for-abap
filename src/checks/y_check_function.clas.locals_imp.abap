CLASS lcl_db_reader IMPLEMENTATION.
  METHOD lif_db_reader~is_fm_rfc_enabled.
    SELECT SINGLE fmode FROM tfdir
      WHERE funcname = @function
        AND fmode EQ 'R'
      INTO @DATA(mode).
    result = xsdbool( sy-subrc EQ 0 ).
  ENDMETHOD.
ENDCLASS.

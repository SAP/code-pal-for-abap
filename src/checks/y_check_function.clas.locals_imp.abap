CLASS lcl_db_reader IMPLEMENTATION.

  METHOD lif_db_reader~is_fm_rfc_enabled.
    SELECT SINGLE @abap_true
    FROM tfdir
    INTO @result
    WHERE funcname = @function
    AND fmode = 'R'.
  ENDMETHOD.

ENDCLASS.

CLASS y_exemption_of_program DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_exemption_of_object.
    ALIASES is_exempted FOR y_if_exemption_of_object~is_exempted.

  PRIVATE SECTION.
    CLASS-DATA name TYPE sobj_name.

    CLASS-METHODS is_enterprise_search_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_downport_assist_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_fin_infotyp_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_irf_model_generate RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_object_sw01_generate RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_exemption_of_program IMPLEMENTATION.


  METHOD y_if_exemption_of_object~is_exempted.
    name = object_name.

    result = xsdbool( is_enterprise_search_generate( ) = abap_true OR
                      is_downport_assist_generate( ) = abap_true OR
                      is_fin_infotyp_generate(  ) = abap_true OR
                      is_irf_model_generate( ) = abap_true OR
                      is_object_sw01_generate( ) ).
  ENDMETHOD.


  METHOD is_downport_assist_generate.
    IF  name(5) = 'NOTE_'.
      result = abap_true.
    ELSE.
      FIND REGEX  '_NOTE_\d{1,20}\>' IN name.
      IF sy-subrc = 0.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_enterprise_search_generate.
    SELECT SINGLE @abap_true
    FROM trdir
    INTO @result
    WHERE name LIKE '%\_001' ESCAPE '\'
    AND ( secu = 'ESH' OR name LIKE 'ESHS%' )
    AND ( subc = 'S' OR subc = '1' )     "include programs ('I') are not supported
    AND name = @name.
  ENDMETHOD.


  METHOD is_fin_infotyp_generate.
    SELECT SINGLE @abap_true
    FROM t777d
    INTO @result
    WHERE repid = @name
    OR btci_prog = @name.
  ENDMETHOD.


  METHOD is_irf_model_generate.
    DATA lv_object_name TYPE vrsd-objname.
    DATA lt_repos_tab TYPE STANDARD TABLE OF abaptxt255.
    DATA lt_trdir_tab TYPE STANDARD TABLE OF trdir.

    CHECK name(10) = 'DTINF_ADJ_' OR name CO '/DTINF_ADJ_CO'.

    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = lv_object_name
        object_type = 'REPS'
        versno      = 0
      TABLES
        repos_tab   = lt_repos_tab
        trdir_tab   = lt_trdir_tab
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF lines( lt_repos_tab ) <= 10.
      RETURN.
    ENDIF.

    READ TABLE lt_repos_tab INTO DATA(repos_tab) INDEX 2.

    IF repos_tab <> '*& Report DTINF_CORR_REPORT'.
      RETURN.
    ENDIF.

    READ TABLE lt_repos_tab INTO repos_tab INDEX 4.

    IF repos_tab <> '*& !!!AUTO-GENERATED CODE: DO NOT CHANGE!!!'.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_object_sw01_generate.
    SELECT SINGLE @abap_true
    FROM tojtb
    INTO @result
    WHERE progname = @name.  "#EC CI_GENBUFF
  ENDMETHOD.


ENDCLASS.

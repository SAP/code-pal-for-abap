CLASS y_exemption_of_program DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES y_if_exemption_of_objects .

protected section.
  PRIVATE SECTION.
    METHODS is_enterprise_search_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_downport_assist_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_fin_infotyp_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.


    METHODS is_irf_model_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.


    METHODS is_object_indepenent_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.


    METHODS is_object_sw01_generate
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS Y_EXEMPTION_OF_PROGRAM IMPLEMENTATION.


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
    SELECT * FROM trdir INTO TABLE @DATA(genprog)
        WHERE name LIKE '%\_001' ESCAPE '\'
          AND ( secu = 'ESH' OR name LIKE 'ESHS%' )
          AND ( subc = 'S' OR subc = '1' )     "include programs ('I') are not supported
          AND name = @name.
    IF sy-subrc EQ 0.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_fin_infotyp_generate.
    SELECT SINGLE repid FROM t777d INTO @DATA(lv_inftype1_progs)
      WHERE repid = @name.
    IF sy-subrc = 0.
      result = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE btci_prog FROM t777d INTO @DATA(lv_inftype2_progs)
      WHERE btci_prog = @name.
    IF sy-subrc = 0.
      result = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD is_irf_model_generate.
    DATA:
      lv_object_name TYPE  vrsd-objname,
      lt_repos_tab   TYPE STANDARD TABLE OF  abaptxt255,
      lw_repos_tab   LIKE LINE OF lt_repos_tab,
      lt_trdir_tab   TYPE STANDARD TABLE OF trdir,
      lv_lines       TYPE i.

    IF name(10) = 'DTINF_ADJ_' OR name CO '/DTINF_ADJ_CO'.

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

      IF sy-subrc = 0.
        DESCRIBE TABLE lt_repos_tab LINES lv_lines.

        IF lv_lines > 10.
          READ TABLE   lt_repos_tab INTO lw_repos_tab INDEX 2.

          IF lw_repos_tab = '*& Report DTINF_CORR_REPORT'.
            READ TABLE   lt_repos_tab INTO lw_repos_tab INDEX 4.

            IF lw_repos_tab = '*& !!!AUTO-GENERATED CODE: DO NOT CHANGE!!!'.
              result = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR lt_repos_tab.
    CLEAR lt_trdir_tab.
  ENDMETHOD.


  METHOD is_object_indepenent_generate.
    DATA: l_object TYPE sobj_name.
    DATA(programming_object) = NEW y_exemption_general( ).
    l_object = name.
    result = programming_object->is_object_exempted( object_type = 'PROG' object_name = name ).
  ENDMETHOD.


  METHOD is_object_sw01_generate.
    SELECT SINGLE progname FROM tojtb  INTO @DATA(l_prog)
      WHERE progname = @name.                           "#EC CI_GENBUFF
    IF sy-subrc = 0.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_exemption_of_objects~is_exempted.
    result = xsdbool( is_enterprise_search_generate( name ) = abap_true OR
                      is_downport_assist_generate( name ) = abap_true OR
                      is_fin_infotyp_generate(  name ) = abap_true OR
                      is_irf_model_generate( name ) = abap_true OR
                      is_object_indepenent_generate(  name ) OR
                      is_object_sw01_generate( name )  ).
  ENDMETHOD.
ENDCLASS.

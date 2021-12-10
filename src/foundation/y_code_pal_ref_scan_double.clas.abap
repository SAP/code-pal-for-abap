CLASS y_code_pal_ref_scan_double DEFINITION PUBLIC.  "#EC INTF_IN_CLASS
  PUBLIC SECTION.
    TYPES source_code TYPE TABLE OF char255 WITH EMPTY KEY.

    CLASS-METHODS get IMPORTING source        TYPE source_code
                      RETURNING VALUE(result) TYPE REF TO cl_ci_scan.

    CLASS-METHODS get_from_global_class IMPORTING name          TYPE trdir-name
                                        RETURNING VALUE(result) TYPE REF TO cl_ci_scan.

    CLASS-METHODS get_from_program IMPORTING name          TYPE trdir-name
                                   RETURNING VALUE(result) TYPE REF TO cl_ci_scan.

    CLASS-METHODS get_from_fuction_group IMPORTING name          TYPE trdir-name
                                         RETURNING VALUE(result) TYPE REF TO cl_ci_scan.

  PROTECTED SECTION.
    CLASS-METHODS create_ref_scan IMPORTING include       TYPE REF TO cl_ci_source_include
                                  RETURNING VALUE(result) TYPE REF TO cl_ci_scan.

    CLASS-METHODS syntax_check IMPORTING source TYPE source_code.

    CLASS-METHODS convert_code IMPORTING source        TYPE source_code
                               RETURNING VALUE(result) TYPE sci_include.

    CLASS-METHODS get_include_from_trdir IMPORTING pattern       TYPE trdir-name
                                         RETURNING VALUE(result) TYPE REF TO cl_ci_source_include.

ENDCLASS.



CLASS y_code_pal_ref_scan_double IMPLEMENTATION.


  METHOD get.
    DATA(include) = cl_ci_source_include=>feed( p_include = convert_code( source )
                                                p_trdir = fake_tadir=>get( ) ).

    result = create_ref_scan( include ).
  ENDMETHOD.


  METHOD get_from_global_class.
    DATA(include) = get_include_from_trdir( |{ name }%=CP| ).
    result = create_ref_scan( include ).
  ENDMETHOD.


  METHOD get_from_program.
    DATA(include) = get_include_from_trdir( name ).
    result = create_ref_scan( include ).
  ENDMETHOD.


  METHOD get_from_fuction_group.
    result = get_from_program( name ).
  ENDMETHOD.


  METHOD syntax_check.
    DATA program TYPE string.
    DATA message TYPE string.
    DATA line  TYPE i.
    DATA word  TYPE string.

    SYNTAX-CHECK FOR source PROGRAM program MESSAGE message LINE line WORD word.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    cl_abap_unit_assert=>fail( msg = 'Syntax Error'
                               detail = | Message:{ message }, Line:{ line }, Word:{ word } | ).
  ENDMETHOD.


  METHOD convert_code.
    syntax_check( source ).
    MOVE-CORRESPONDING source TO result.
  ENDMETHOD.


  METHOD create_ref_scan.
    CONSTANTS class_type TYPE string VALUE 'CL_CI_SCAN'.

    DATA(parameters) = VALUE abap_parmbind_tab( ( name  = 'P_INCLUDE'
                                                  kind  = cl_abap_objectdescr=>exporting
                                                  value = REF #( include ) )
                                                ( name  = 'P_NO_CLASSIFICATION'
                                                  kind  = cl_abap_objectdescr=>exporting
                                                  value = REF #( abap_true ) )
                                                ( name  = 'P_NOAUNIT'
                                                  kind  = cl_abap_objectdescr=>exporting
                                                  value = REF #( abap_true ) ) ).

    CATCH SYSTEM-EXCEPTIONS dyn_call_meth_param_not_found = 1.
      CREATE OBJECT result TYPE (class_type) PARAMETER-TABLE parameters.
    ENDCATCH.

    IF sy-subrc = 1.
      DELETE parameters WHERE name = 'P_NO_CLASSIFICATION'.
      CREATE OBJECT result TYPE (class_type) PARAMETER-TABLE parameters.
    ENDIF.
  ENDMETHOD.


  METHOD get_include_from_trdir.
    DATA name TYPE trdir-name.

    IF pattern CS '%'.
      SELECT SINGLE name
      FROM trdir
      INTO @name
      WHERE name LIKE @pattern.
    ELSE.
      name = pattern.
    ENDIF.

    result = cl_ci_source_include=>create( p_name = name ).
  ENDMETHOD.


ENDCLASS.

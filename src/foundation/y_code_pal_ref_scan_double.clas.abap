CLASS y_code_pal_ref_scan_double DEFINITION PUBLIC. "#EC INTF_IN_CLASS
  PUBLIC SECTION.
    CONSTANTS unit_test_identifier TYPE trdir-name VALUE 'CODE_PAL_FOR_ABAP_UNIT_TEST' ##NO_TEXT.

    CLASS-METHODS get IMPORTING source        TYPE y_char255_tab
                      RETURNING VALUE(result) TYPE REF TO cl_ci_scan.

  PROTECTED SECTION.
    CLASS-METHODS create_ref_scan IMPORTING include       TYPE REF TO cl_ci_source_include
                                  RETURNING VALUE(result) TYPE REF TO cl_ci_scan.

    CLASS-METHODS syntax_check IMPORTING source TYPE y_char255_tab.

    CLASS-METHODS convert_code IMPORTING source        TYPE y_char255_tab
                               RETURNING VALUE(result) TYPE sci_include.

    CLASS-METHODS create_trdir RETURNING VALUE(result) TYPE trdir.

ENDCLASS.



CLASS Y_CODE_PAL_REF_SCAN_DOUBLE IMPLEMENTATION.


  METHOD get.
    DATA(trdir) = create_trdir( ).

    syntax_check( source ).
    DATA(source_code) = convert_code( source ).

    DATA(include) = cl_ci_source_include=>feed( p_include = source_code
                                                p_trdir = trdir ).

    result = create_ref_scan( include  ).
    result->determine_aunit_lines( ).
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


  METHOD create_trdir.
    result = VALUE #( name = unit_test_identifier ).
  ENDMETHOD.


ENDCLASS.

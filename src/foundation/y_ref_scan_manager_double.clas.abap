CLASS y_ref_scan_manager_double DEFINITION PUBLIC INHERITING FROM y_ref_scan_manager. "#EC INTF_IN_CLASS
  PUBLIC SECTION.
    CONSTANTS unit_test_identifier TYPE trdir-name VALUE 'CODE_PAL_FOR_ABAP_UNIT_TEST' ##NO_TEXT.

    METHODS y_if_scan_manager~set_ref_scan REDEFINITION.
    METHODS inject_code IMPORTING source TYPE y_char255_tab.

  PROTECTED SECTION.
    METHODS create_ref_scan IMPORTING include       TYPE REF TO cl_ci_source_include
                            RETURNING VALUE(result) TYPE REF TO cl_ci_scan .

    METHODS syntax_check IMPORTING source TYPE y_char255_tab.

    METHODS convert_code IMPORTING source        TYPE y_char255_tab
                         RETURNING VALUE(result) TYPE sci_include.

    METHODS create_trdir RETURNING VALUE(result) TYPE trdir.

  PRIVATE SECTION.
    DATA source_code TYPE sci_include.

ENDCLASS.



CLASS Y_REF_SCAN_MANAGER_DOUBLE IMPLEMENTATION.


  METHOD y_if_scan_manager~set_ref_scan.
    DATA(trdir) = create_trdir( ).

    DATA(include) = cl_ci_source_include=>feed( p_include = source_code
                                                p_trdir = trdir ).

    DATA(ref_scan) = create_ref_scan( include  ).

    super->y_if_scan_manager~set_ref_scan( ref_scan ).
  ENDMETHOD.


  METHOD inject_code.
    syntax_check( source ).
    source_code = convert_code( source ).
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

CLASS y_ref_scan_manager_double DEFINITION PUBLIC INHERITING FROM y_ref_scan_manager.
  PUBLIC SECTION.
    METHODS set_ref_scan REDEFINITION.
    METHODS inject_code IMPORTING source TYPE y_char255_tab.
  PROTECTED SECTION.
    METHODS create_ref_scan IMPORTING include TYPE REF TO cl_ci_source_include
                            RETURNING VALUE(result) TYPE REF TO cl_ci_scan .
    METHODS syntax_check IMPORTING source TYPE y_char255_tab.
    METHODS convert_code IMPORTING source TYPE y_char255_tab
                         RETURNING VALUE(result) TYPE sci_include.
  PRIVATE SECTION.
    DATA source_code TYPE sci_include.
ENDCLASS.



CLASS Y_REF_SCAN_MANAGER_DOUBLE IMPLEMENTATION.

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

  METHOD inject_code.
    syntax_check( source ).
    source_code = convert_code( source ).
  ENDMETHOD.

  METHOD convert_code.
    MOVE-CORRESPONDING source TO result.
  ENDMETHOD.

  METHOD set_ref_scan.
    DATA(include) = cl_ci_source_include=>feed( source_code ).
    DATA(ref_scan) = create_ref_scan( include ).
    super->set_ref_scan( ref_scan ).
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

    TRY.
        CREATE OBJECT result TYPE (class_type) PARAMETER-TABLE parameters.
      CATCH cx_root.
        DELETE parameters WHERE name = 'P_NO_CLASSIFICATION'.
        CREATE OBJECT result TYPE (class_type) PARAMETER-TABLE parameters.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

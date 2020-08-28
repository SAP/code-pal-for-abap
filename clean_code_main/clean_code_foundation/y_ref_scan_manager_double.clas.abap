CLASS y_ref_scan_manager_double DEFINITION PUBLIC INHERITING FROM y_ref_scan_manager.
  PUBLIC SECTION.
    TYPES str TYPE c LENGTH 255.
    TYPES strtab TYPE STANDARD TABLE OF str.
    METHODS set_ref_scan REDEFINITION.
  PROTECTED SECTION.
    METHODS inject_code IMPORTING source TYPE strtab.
  PRIVATE SECTION.
    DATA source_code TYPE sci_include.
    METHODS syntax_check IMPORTING source TYPE strtab.
    METHODS convert_code IMPORTING source TYPE strtab
                         RETURNING VALUE(result) TYPE sci_include.
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
    DATA(source_include) = cl_ci_source_include=>feed( source_code ).

    DATA(ref_scan) = new cl_ci_scan( p_include = source_include
                                     p_no_classification = abap_true
                                     p_noaunit = abap_true
                                     p_no_cache = abap_true ).

    super->set_ref_scan( ref_scan ).
  ENDMETHOD.

ENDCLASS.

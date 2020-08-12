CLASS y_scan_manager_double DEFINITION FOR TESTING PUBLIC DURATION SHORT RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    TYPES str TYPE c LENGTH 255.
    TYPES strtab TYPE STANDARD TABLE OF str.

    INTERFACES: y_if_scan_manager.
    ALIASES get_structures FOR y_if_scan_manager~get_structures.
    ALIASES get_statements FOR y_if_scan_manager~get_statements.
    ALIASES get_tokens FOR y_if_scan_manager~get_tokens.
    ALIASES get_levels FOR y_if_scan_manager~get_levels.
    ALIASES set_ref_scan FOR y_if_scan_manager~set_ref_scan.
    ALIASES is_scan_ok FOR y_if_scan_manager~is_scan_ok.
  PROTECTED SECTION.
    DATA levels TYPE slevel_tab.
    DATA structures TYPE sstruc_tab.
    DATA statements TYPE sstmnt_tab.
    DATA tokens TYPE stokesx_tab.

    METHODS convert_code IMPORTING source TYPE strtab.
    METHODS code_syntax_check IMPORTING source TYPE strtab.
  PRIVATE SECTION.
ENDCLASS.



CLASS Y_SCAN_MANAGER_DOUBLE IMPLEMENTATION.


  METHOD y_if_scan_manager~get_structures.
    result = structures.
  ENDMETHOD.


  METHOD y_if_scan_manager~get_statements.
    result = statements.
  ENDMETHOD.


  METHOD y_if_scan_manager~get_tokens.
    result = tokens.
  ENDMETHOD.


  METHOD y_if_scan_manager~get_levels.
    result = levels.
  ENDMETHOD.


  METHOD y_if_scan_manager~set_ref_scan.
    RETURN.
  ENDMETHOD.


  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.


  METHOD code_syntax_check.
    DATA program TYPE string.
    DATA message TYPE string.
    DATA line  TYPE i.
    DATA word  TYPE string.

    SYNTAX-CHECK FOR source PROGRAM program MESSAGE message LINE line WORD word.

    IF sy-subrc NE 0.
      cl_abap_unit_assert=>fail( msg = 'Syntax Error'
                                 detail = | Message:{ message }, Line:{ line }, Word:{ word } |
                               ).
    ENDIF.
  ENDMETHOD.


  METHOD convert_code.
    code_syntax_check( source ).

    SCAN ABAP-SOURCE source
    LEVELS INTO levels
    STRUCTURES INTO structures
    STATEMENTS INTO statements
    TOKENS INTO tokens
    WITH ANALYSIS
    WITH COMMENTS.
  ENDMETHOD.
ENDCLASS.

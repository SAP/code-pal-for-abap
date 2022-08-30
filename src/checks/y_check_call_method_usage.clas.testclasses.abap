CLASS ltc_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_call_method_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'CLASS lcl_classname DEFINITION. ' )
      ( '  PUBLIC SECTION. ' )
      ( '    METHODS class_method. ' )
      ( '    CLASS-METHODS execute. ' )
      ( 'ENDCLASS. ' )

      ( 'CLASS lcl_classname IMPLEMENTATION. ' )
      ( '  METHOD class_method. ' )
      ( '    CALL METHOD  lcl_classname=>execute( ). ' )
      ( '  ENDMETHOD. ' )
      ( '  METHOD execute. ' )
      ( '  ENDMETHOD. ' )
      ( 'ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'CLASS lcl_classname DEFINITION. ' )
      ( '  PUBLIC SECTION. ' )
      ( '    METHODS class_method. ' )
      ( '    CLASS-METHODS execute. ' )
      ( 'ENDCLASS. ' )

      ( 'CLASS lcl_classname IMPLEMENTATION. ' )
      ( '  METHOD class_method. ' )
      ( '    lcl_classname=>execute( ). ' )
      ( |    CALL METHOD ('execute'). | )
      ( '  ENDMETHOD. ' )
      ( '  METHOD execute. ' )
      ( '  ENDMETHOD. ' )
      ( 'ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'CLASS lcl_classname DEFINITION. ' )
      ( '  PUBLIC SECTION. ' )
      ( '    METHODS class_method. ' )
      ( '    CLASS-METHODS execute. ' )
      ( 'ENDCLASS. ' )

      ( 'CLASS lcl_classname IMPLEMENTATION. ' )
      ( '  METHOD class_method. ' )
      ( '    CALL METHOD  lcl_classname=>execute( ). "#EC CALL_METH_USAGE ' )
      ( '  ENDMETHOD. ' )
      ( '  METHOD execute. ' )
      ( '  ENDMETHOD. ' )
      ( 'ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_report DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_report IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_call_method_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'CLASS lcl_classname DEFINITION. ' )
      ( '  PUBLIC SECTION. ' )
      ( '    CLASS-METHODS execute. ' )
      ( 'ENDCLASS. ' )

      ( 'CLASS lcl_classname IMPLEMENTATION. ' )
      ( '  METHOD execute. ' )
      ( '  ENDMETHOD. ' )
      ( 'ENDCLASS. ' )

      ( '  START-OF-SELECTION. ' )
      ( '    DATA cn TYPE REF TO lcl_classname. ' )
      ( '    cn = NEW lcl_classname( ). ' )
      ( '    CALL METHOD cn->execute. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'CLASS lcl_classname DEFINITION. ' )
      ( '  PUBLIC SECTION. ' )
      ( '    CLASS-METHODS execute. ' )
      ( 'ENDCLASS. ' )

      ( 'CLASS lcl_classname IMPLEMENTATION. ' )
      ( '  METHOD execute. ' )
      ( '  ENDMETHOD. ' )
      ( 'ENDCLASS. ' )

      ( '  START-OF-SELECTION. ' )
      ( '    DATA cn TYPE REF TO lcl_classname. ' )
      ( '    cn = NEW lcl_classname( ). ' )
      ( |    CALL METHOD cn->('execute'). | )
      ( |    CALL METHOD lcl_classname=>('execute'). | )
      ( |    CALL METHOD ('lcl_classname')=>('execute'). | )
      ( |    CALL METHOD ('lcl_classname')=>execute. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'CLASS lcl_classname DEFINITION. ' )
      ( '  PUBLIC SECTION. ' )
      ( '    CLASS-METHODS execute. ' )
      ( '  PROTECTED SECTION. ' )
      ( 'ENDCLASS. ' )

      ( 'CLASS lcl_classname IMPLEMENTATION. ' )
      ( '  METHOD execute. ' )
      ( '  ENDMETHOD. ' )
      ( 'ENDCLASS. ' )

      ( '  START-OF-SELECTION. ' )
      ( '    DATA cn TYPE REF TO lcl_classname. ' )
      ( '    cn = NEW lcl_classname( ). ' )
      ( '    CALL METHOD cn->execute. "#EC CALL_METH_USAGE ' )
    ).
  ENDMETHOD.

ENDCLASS.

class ltc_ole_calls DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_ole_calls IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = value #(
      ( 'REPORT y_example.' )

      ( 'START-OF-SELECTION.' )
      ( '  DATA excel TYPE ole2_object.' )
      ( '  CALL METHOD OF excel ''AppClose''.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    cl_abap_unit_assert=>skip( 'Not implemented' ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    cl_abap_unit_assert=>skip( 'Not implemented.' ).
  ENDMETHOD.

  METHOD get_cut.
    result = new y_check_call_method_usage( ).
  ENDMETHOD.

ENDCLASS.

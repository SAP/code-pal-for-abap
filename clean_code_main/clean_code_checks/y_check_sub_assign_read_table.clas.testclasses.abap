CLASS local_test_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_sub_assign_read_table( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '     DATA clients TYPE TABLE OF t000. ')
      ( '     READ TABLE clients ASSIGNING FIELD-SYMBOL(<client>) INDEX 1. ' )
      ( '     DATA(i) = 1. ' )
      ( '     READ TABLE clients ASSIGNING <client> INDEX 2. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '     DATA clients TYPE TABLE OF t000. ')
      ( '     READ TABLE clients INTO DATA(client) INDEX 1. ' )
      ( '     READ TABLE clients ASSIGNING FIELD-SYMBOL(<client>) INDEX 1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '     DATA clients TYPE TABLE OF t000. ')
      ( '     READ TABLE clients ASSIGNING FIELD-SYMBOL(<client>) INDEX 1. ' )
      ( '     DATA(i) = 1. ' )
      ( '     READ TABLE clients ASSIGNING <client> INDEX 2. "#EC SUB_ASSIGN' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

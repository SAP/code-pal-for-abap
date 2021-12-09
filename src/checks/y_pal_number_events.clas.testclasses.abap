CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_number_events( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-EVENTS one. ' )
      ( '     EVENTS two. ' )
      ( '     CLASS-EVENTS three. ' )
      ( '     EVENTS four. ' )
      ( '     EVENTS five. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-EVENTS one. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. "#EC NUMBER_EVENTS ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-EVENTS one. ' )
      ( '     EVENTS two. ' )
      ( '     CLASS-EVENTS three. ' )
      ( '     EVENTS four. ' )
      ( '     EVENTS five. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

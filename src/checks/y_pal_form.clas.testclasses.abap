CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_form( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   PERFORM example. ' )

      ( ' FORM example. ' )
      ( '   DATA class TYPE REF TO cl_oo_class.' )
      ( ' ENDFORM.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '     DATA class TYPE REF TO cl_oo_class.' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   PERFORM example. ' )

      ( ' FORM example. "#EC CI_FORM ' )
      ( '   DATA class TYPE REF TO cl_oo_class.' )
      ( ' ENDFORM. ' )
    ).
  ENDMETHOD.

ENDCLASS.

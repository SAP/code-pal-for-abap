CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_receiving_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS test RETURNING VALUE(receiving) TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(example) = NEW y_example_class( ). ' )
      ( |   example->test( RECEIVING receiving = DATA(receiving) ). | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS test RETURNING VALUE(receiving) TYPE string ' )
      ( '                  EXCEPTIONS exceptions.' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '     RAISE exceptions.' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(example) = NEW y_example_class( ). ' )
      ( |   DATA(receiving) = example->test( ). | )
      ( |   example->test( RECEIVING receiving = receiving | )
      ( |                  EXCEPTIONS exceptions = 4 ). | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS test RETURNING VALUE(name) TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(example) = NEW y_example_class( ). ' )
      ( |   example->test( RECEIVING name = DATA(name) ). "#EC RECEIVING_USAGE | )
    ).
  ENDMETHOD.

ENDCLASS.

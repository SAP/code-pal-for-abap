CLASS local_test_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut                 REDEFINITION.
    METHODS get_code_with_issue     REDEFINITION.
    METHODS get_code_without_issue  REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.


CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_missing_ut_assert( ).
  ENDMETHOD.


  METHOD get_code_with_issue.
    result = VALUE #( ( 'REPORT y_example. ' )
                      ( ' CLASS y_example_class DEFINITION FOR TESTING. ' )
                      ( '   PUBLIC SECTION. ' )
                      ( '   PROTECTED SECTION. ' )
                      ( '     METHODS test FOR TESTING. ' )
                      ( ' ENDCLASS. ' )

                      ( ' CLASS y_example_class IMPLEMENTATION. ' )
                      ( '   METHOD test. ' )
                      ( '     RETURN. ' )
                      ( '   ENDMETHOD. ' )
                      ( ' ENDCLASS. ' ) ).
  ENDMETHOD.


  METHOD get_code_without_issue.
    result = VALUE #( ( 'REPORT y_example. ' )
                      ( ' CLASS y_example_class DEFINITION FOR TESTING. ' )
                      ( '   PUBLIC SECTION. ' )
                      ( '   PROTECTED SECTION. ' )
                      ( '     METHODS test FOR TESTING. ' )
                      ( ' ENDCLASS. ' )

                      ( ' CLASS y_example_class IMPLEMENTATION. ' )
                      ( '   METHOD test. ' )
                      ( '     CL_ABAP_UNIT_ASSERT=>ASSERT_BOUND( act = me ). ' )
                      ( '   ENDMETHOD. ' )
                      ( ' ENDCLASS. ' ) ).
  ENDMETHOD.


  METHOD get_code_with_exemption.
    result = VALUE #( ( 'REPORT y_example. ' )
                      ( ' CLASS y_example_class DEFINITION FOR TESTING. ' )
                      ( '   PUBLIC SECTION. ' )
                      ( '   PROTECTED SECTION. ' )
                      ( '     METHODS test FOR TESTING.  "#EC MISS_UT_ASSERT ' )
                      ( ' ENDCLASS. ' )

                      ( ' CLASS y_example_class IMPLEMENTATION. ' )
                      ( '   METHOD test. ' )
                      ( '     RETURN. ' )
                      ( '   ENDMETHOD. ' )
                      ( ' ENDCLASS. ' ) ).
  ENDMETHOD.

ENDCLASS.

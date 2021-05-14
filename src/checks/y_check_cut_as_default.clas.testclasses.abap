CLASS ltc_cut DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_cut IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_cut_as_default( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     " given ' )
      ( '     DATA demo_failures TYPE REF TO y_demo_failures. ' )
      ( '     " when ' )
      ( '     demo_failures = NEW #( ). ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_bound( demo_failures ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     " given ' )
      ( '     DATA cut TYPE REF TO y_demo_failures. ' )
      ( '     " when ' )
      ( '     cut = NEW #( ). ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_bound( cut ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. "#EC CUT_AS_DEFAULT ' )
      ( '     " given ' )
      ( '     DATA demo_failures TYPE REF TO y_demo_failures. ' )
      ( '     " when ' )
      ( '     demo_failures = NEW #( ). ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_bound( demo_failures ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_prefix DEFINITION INHERITING FROM ltc_cut FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_prefix IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     " given ' )
      ( '     DATA lo_cut TYPE REF TO y_demo_failures. ' )
      ( '     " when ' )
      ( '     lo_cut = NEW #( ). ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_bound( lo_cut ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_not_for_testing DEFINITION INHERITING FROM ltc_cut FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_not_for_testing IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_attribute DEFINITION INHERITING FROM ltc_cut FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_attribute IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA lo_cut TYPE REF TO y_demo_failures. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     " when ' )
      ( '     lo_cut = NEW #( ). ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_bound( lo_cut ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_cut_out_of_testing_method DEFINITION INHERITING FROM ltc_cut FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_cut_out_of_testing_method IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS when. ' )
      ( '     METHODS then. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA cut TYPE REF TO y_demo_failures. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     when( ).' )
      ( '     then( ).' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD when. ' )
      ( '     cut = NEW #( ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD then. ' )
      ( '     cl_abap_unit_assert=>assert_bound( cut ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

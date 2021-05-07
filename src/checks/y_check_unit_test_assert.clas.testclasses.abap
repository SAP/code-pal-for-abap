CLASS ltc_same_variable DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_same_variable IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_unit_test_assert( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS sum FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD sum. ' )
      ( '     " given ' )
      ( '     DATA(first) = 10. ' )
      ( '     DATA(second) = 10. ' )
      ( '     " when ' )
      ( '     DATA(sum) = first + second. ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_equals( act = sum  ' )
      ( '                                         exp = sum ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS sum FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD sum. ' )
      ( '     " given ' )
      ( '     DATA(first) = 10. ' )
      ( '     DATA(second) = 10. ' )
      ( '     " when ' )
      ( '     DATA(sum) = first + second. ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_equals( act = sum  ' )
      ( '                                         exp = 20 ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS sum FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD sum. ' )
      ( '     " given ' )
      ( '     DATA(first) = 10. ' )
      ( '     DATA(second) = 10. ' )
      ( '     " when ' )
      ( '     DATA(sum) = first + second. ' )
      ( '     " then ' )
      ( '     cl_abap_unit_assert=>assert_equals( act = sum  ' )
      ( '                                         exp = sum ). "#EC UT_ASSERT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_hardcoded_number DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_hardcoded_number IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_unit_test_assert( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS sum FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD sum. ' )
      ( '     " given ' )
      ( '     DATA(first) = 10. ' )
      ( '     DATA(second) = 10. ' )
      ( '     " when ' )
      ( '     DATA(sum) = first + second. ' )
      ( '     " then ' )
      ( '     cl_aunit_assert=>assert_differs( act = 10  ' )
      ( '                                      exp = 20 ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS sum FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD sum. ' )
      ( '     " given ' )
      ( '     DATA(first) = 10. ' )
      ( '     DATA(second) = 10. ' )
      ( '     " when ' )
      ( '     DATA(sum) = first + second. ' )
      ( '     " then ' )
      ( '     cl_aunit_assert=>assert_differs( act = 10  ' )
      ( '                                      exp = sum ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS sum FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD sum. ' )
      ( '     " given ' )
      ( '     DATA(first) = 10. ' )
      ( '     DATA(second) = 10. ' )
      ( '     " when ' )
      ( '     DATA(sum) = first + second. ' )
      ( '     " then ' )
      ( '     cl_aunit_assert=>assert_differs( act = 10  ' )
      ( '                                      exp = 20 ).  "#EC UT_ASSERT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_hardcoded_string DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_hardcoded_string IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_unit_test_assert( ).
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
      ( |     cl_aunit_assert=>assert_differs( act = 'A' | )
      ( |                                      exp = 'B' ). | )
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
      ( '     DATA(system) = sy-sysid. ' )
      ( |     cl_aunit_assert=>assert_differs( act = system | )
      ( |                                      exp = 'B' ). | )
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
      ( '   METHOD example. ' )
      ( |     cl_aunit_assert=>assert_differs( act = 'A' | )
      ( |                                      exp = 'B' ). "#EC UT_ASSERT | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_public_classdata DEFINITION INHERITING FROM ltc_hardcoded_string FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_public_classdata IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '     CLASS-DATA system TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     cl_aunit_assert=>assert_differs( act = system | )
      ( |                                      exp = 'B' ). | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_protected_data DEFINITION INHERITING FROM ltc_hardcoded_string FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_protected_data IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA system TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     cl_aunit_assert=>assert_differs( act = system | )
      ( |                                      exp = 'system' ). | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_comments DEFINITION INHERITING FROM ltc_same_variable FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_comments IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA system TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |*     cl_aunit_assert=>assert_differs( act = system | )
      ( |*                                      exp = system ). | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_non_standard_assert DEFINITION INHERITING FROM ltc_same_variable FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_non_standard_assert IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_unit_test_assert( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_assert DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS assert IMPORTING VALUE(exp) TYPE any ' )
      ( '                                    VALUE(act) TYPE any ' )
      ( '                          RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_assert IMPLEMENTATION. ' )
      ( '   METHOD assert. ' )
      ( '     result = cl_abap_unit_assert=>assert_equals( exp = exp ' )
      ( '                                                  act = act  ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA system TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     y_assert=>assert( act = system ' )
      ( '                       exp = system ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_assert DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS assert IMPORTING VALUE(exp) TYPE any ' )
      ( '                                    VALUE(act) TYPE any ' )
      ( '                          RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_assert IMPLEMENTATION. ' )
      ( '   METHOD assert. ' )
      ( '     result = cl_abap_unit_assert=>assert_equals( exp = exp ' )
      ( '                                                  act = act  ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA system TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     y_assert=>assert( act = sy-sysid ' )
      ( '                       exp = system ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_assert DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS assert IMPORTING VALUE(exp) TYPE any ' )
      ( '                                    VALUE(act) TYPE any ' )
      ( '                          RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_assert IMPLEMENTATION. ' )
      ( '   METHOD assert. ' )
      ( '     result = cl_abap_unit_assert=>assert_equals( exp = exp ' )
      ( '                                                  act = act  ). ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA system TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     y_assert=>assert( act = system ' )
      ( '                       exp = system ). "#EC UT_ASSERT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.


ENDCLASS.



CLASS ltc_lines DEFINITION INHERITING FROM ltc_hardcoded_string FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_lines IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA atc_tadir TYPE TABLE OF tadir. ' )
      ( '     DATA exp_tadir TYPE TABLE OF tadir. ' )
      ( |     cl_aunit_assert=>assert_equals( act = lines( atc_tadir ) | )
      ( |                                     exp = lines( exp_tadir ) ). | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_call_static DEFINITION INHERITING FROM ltc_hardcoded_string FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_call_static IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_fake DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS get_fullname IMPORTING name TYPE string ' )
      ( '                                          surname TYPE string ' )
      ( '                                RETURNING VALUE(result) TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_fake IMPLEMENTATION. ' )
      ( '   METHOD get_fullname. ' )
      ( '     result = |{ name } { surname }|. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )


      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     cl_aunit_assert=>assert_equals( act = y_fake=>get_fullname( name = 'code pal' surname = 'for ABAP' ) | )
      ( |                                     exp = y_fake=>get_fullname( name = 'code pal' | )
      ( |                                                                 surname = 'for ABAP' ) ). | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_assert_fail DEFINITION INHERITING FROM ltc_hardcoded_string FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_assert_fail IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     cl_aunit_assert=>fail( msg = 'method ended unexpectedly' ). | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_assert_empty DEFINITION INHERITING FROM ltc_hardcoded_string FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_assert_empty IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     cl_aunit_assert=>assert_equals( act = ''  exp = '' ). | )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_if DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_if IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_magic_number( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA val TYPE i VALUE 1.' )
      ( '   IF val EQ 8.' )
      ( '     val = 2.' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   CONSTANTS active TYPE i VALUE 2. ' )
      ( '   CONSTANTS inactive TYPE i VALUE 1. ' )
      ( '   CONSTANTS not_set TYPE i VALUE 0. ' )
      ( '   DATA(val) = active.' )
      ( '   IF val EQ not_set.' )
      ( '     val = inactive.' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA(val) = 1.' )
      ( '   IF val EQ 2. "#EC CI_MAGIC ' )
      ( '     val = 0.' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_do DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_do IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_magic_number( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DO 5 TIMES.' )
      ( '     "Try Something ' )
      ( '   ENDDO.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   CONSTANTS attempts TYPE i VALUE 5. ' )
      ( '   DO attempts TIMES.' )
      ( '     "Try Something ' )
      ( '   ENDDO.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DO 5 TIMES. "#EC CI_MAGIC ' )
      ( '     "Try Something ' )
      ( '   ENDDO. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_check DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_check IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_magic_number( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA(skip) = 1.' )
      ( '   CHECK skip = 2. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA(skip) = abap_false.' )
      ( '   CHECK skip = abap_true.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA(skip) = 1.' )
      ( '   CHECK skip = 2. "#EC CI_MAGIC' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_numeric_string DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_numeric_string IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_magic_number( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA(skip) = '20'. | )
      ( |   CHECK skip = '30'. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA(skip) = abap_false.' )
      ( '   CHECK skip = abap_true.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA(skip) = '20'. | )
      ( |   CHECK skip = '30'. "#EC CI_MAGIC' | )
    ).
  ENDMETHOD.

ENDCLASS.

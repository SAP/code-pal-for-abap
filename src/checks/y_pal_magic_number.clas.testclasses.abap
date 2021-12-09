CLASS ltc_if DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_if IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_magic_number( ).
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


CLASS ltc_do DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_do IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_magic_number( ).
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


CLASS ltc_check DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_check IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_magic_number( ).
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


CLASS ltc_numeric_string DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_numeric_string IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_magic_number( ).
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


CLASS ltc_case DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_case IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_magic_number( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA(position) = 10. | )
      ( |   CASE position. | )
      ( |     WHEN 8. | )
      ( |       WRITE 'ok'. | )
      ( |   ENDCASE. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   CONSTANTS first TYPE i VALUE 1. | )
      ( |   DATA(position) = 10. | )
      ( |   CASE position. | )
      ( |     WHEN first. | )
      ( |       WRITE 'ok'. | )
      ( |   ENDCASE. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA(position) = 10. | )
      ( |   CASE position. | )
      ( |     WHEN 8. "#EC CI_MAGIC | )
      ( |       WRITE 'ok'. | )
      ( |   ENDCASE. | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_sy DEFINITION INHERITING FROM ltc_if FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_sy IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   CHECK sy-subrc = 4. ' )

      ( '   IF sy-subrc = 0. ' )
      ( '   ENDIF. ' )

      ( |   CASE sy-subrc. | )
      ( |     WHEN 0. | )
      ( |       WRITE 'ok'. | )
      ( |     WHEN 4. | )
      ( |       WRITE 'not found'. | )
      ( |     WHEN 8. | )
      ( |       WRITE 'other error'. | )
      ( |   ENDCASE. | )

      ( |   CASE sy-tabix. | )
      ( |     WHEN 1. | )
      ( |       WRITE 'first'. | )
      ( |     WHEN 2. | )
      ( |       WRITE 'second'. | )
      ( |     WHEN 3. | )
      ( |       WRITE 'other'. | )
      ( |   ENDCASE. | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_index DEFINITION INHERITING FROM ltc_if FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_index IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA tokens TYPE stokesx_tab. ' )

      ( '   IF tokens[ 3 + 5 ] IS INITIAL. ' )
      ( '   ENDIF. ' )

      ( '   DATA(first) = tokens[ 4 ]. ' )
      ( '   IF tokens[ line_index( tokens[ table_line = first ] ) + 1 ] IS INITIAL. ' )
      ( '   ENDIF. ' )

      ( '   IF sy-tabix = 10. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_lines DEFINITION INHERITING FROM ltc_if FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_lines IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA tokens TYPE stokesx_tab. ' )
      ( '   IF lines( tokens ) = 10. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_empty_string DEFINITION INHERITING FROM ltc_if FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_empty_string IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   IF '' = ''. | )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_leading_zeros DEFINITION INHERITING FROM ltc_if FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_leading_zeros IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA char TYPE c LENGTH 6. | )
      ( |   IF char = '000000'. | )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_checking_numbers DEFINITION INHERITING FROM ltc_if FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_checking_numbers IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA char TYPE c LENGTH 6. | )
      ( |   IF char CO '0123456789'. | )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

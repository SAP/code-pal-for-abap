CLASS ltc_if DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_if IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_scope_of_variable( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     DATA(var2) = abap_true. ' )
      ( '   ENDIF. ' )

      ( '   var2 = abap_false. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     DATA(var2) = abap_true. ' )
      ( '     var1 = var2. ' )
      ( '   ENDIF. ' )

      ( '   var1 = abap_false. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     DATA(var2) = abap_true. ' )
      ( '   ENDIF. ' )

      ( '   var2 = abap_false. "#EC SCOPE_OF_VAR ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_elseif DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_elseif IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_scope_of_variable( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     DATA(var2) = abap_true. ' )
      ( '   ELSEIF sy-mandt = 200. ' )
      ( '     var2 = abap_false. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     var1 = abap_true. ' )
      ( '   ELSEIF sy-mandt = 200. ' )
      ( '     var1 = abap_false. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     DATA(var2) = abap_true. ' )
      ( '   ELSEIF sy-mandt = 200. ' )
      ( '     var2 = abap_false. "#EC SCOPE_OF_VAR ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_else DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_else IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_scope_of_variable( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     var1 = abap_true. ' )
      ( '   ELSE. ' )
      ( '     DATA(var2) = abap_false. ' )
      ( '   ENDIF. ' )

      ( '   var1 = var2. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     var1 = abap_true. ' )
      ( '   ELSE. ' )
      ( '     var1 = abap_false. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(var1) = abap_false. ' )

      ( '   IF sy-mandt = 100. ' )
      ( '     var1 = abap_true. ' )
      ( '   ELSE. ' )
      ( '     DATA(var2) = abap_false. ' )
      ( '   ENDIF. ' )

      ( '   var1 = var2. "#EC SCOPE_OF_VAR' )
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
    result ?= NEW y_pal_scope_of_variable( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CASE sy-mandt. ' )
      ( '     WHEN 100. ' )
      ( '       DATA(var) = 10. ' )
      ( '     WHEN 200. ' )
      ( '       var = 12. ' )
      ( '   ENDCASE. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA var TYPE i. ' )

      ( '   CASE sy-mandt. ' )
      ( '     WHEN 100. ' )
      ( '       var = 10. ' )
      ( '     WHEN 200. ' )
      ( '       var = 12. ' )
      ( '   ENDCASE. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CASE sy-mandt. ' )
      ( '     WHEN 100. ' )
      ( '       DATA(var) = 10. ' )
      ( '     WHEN 200. ' )
      ( '       var = 12. "#EC SCOPE_OF_VAR' )
      ( '   ENDCASE. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_loop DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_loop IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_scope_of_variable( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )

      ( '   LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>). ' )
      ( '     DATA(object) = <tadir>-object. ' )
      ( '   ENDLOOP. ' )

      ( '   IF object IS INITIAL. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( '   DATA object TYPE trobjtype. ' )

      ( '   LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>). ' )
      ( '     object = <tadir>-object. ' )
      ( '   ENDLOOP. ' )

      ( '   IF object IS INITIAL. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )

      ( '   LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>). ' )
      ( '     DATA(object) = <tadir>-object. ' )
      ( '   ENDLOOP. ' )

      ( '   IF object IS INITIAL. "#EC SCOPE_OF_VAR' )
      ( '   ENDIF. ' )
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
    result ?= NEW y_pal_scope_of_variable( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )

      ( '   DO 10 TIMES. ' )
      ( '     DATA(tabix) = sy-tabix. ' )
      ( '   ENDDO. ' )

      ( '   IF tabix = 10. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( '   DATA tabix LIKE sy-tabix. ' )

      ( '   DO 10 TIMES. ' )
      ( '     tabix = sy-tabix. ' )
      ( '   ENDDO. ' )

      ( '   IF tabix = 10. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )

      ( '   DO 10 TIMES. ' )
      ( '     DATA(tabix) = sy-tabix. ' )
      ( '   ENDDO. ' )

      ( '   IF tabix = 10. "#EC SCOPE_OF_VAR' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_while DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_while IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_scope_of_variable( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA count TYPE i VALUE 10. ' )

      ( '   WHILE count > 0. ' )
      ( '     DATA(tabix) = sy-tabix. ' )
      ( '     count = count - 1. ' )
      ( '   ENDWHILE. ' )

      ( '   IF tabix = 10. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA count TYPE i VALUE 10. ' )
      ( '   DATA tabix LIKE sy-tabix. ' )

      ( '   WHILE count > 0. ' )
      ( '     tabix = sy-tabix. ' )
      ( '     count = count - 1. ' )
      ( '   ENDWHILE. ' )

      ( '   IF tabix = 10. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA count TYPE i VALUE 10. ' )

      ( '   WHILE count > 0. ' )
      ( '     DATA(tabix) = sy-tabix. ' )
      ( '     count = count - 1. ' )
      ( '   ENDWHILE. ' )

      ( '   IF tabix = 10. "#EC SCOPE_OF_VAR' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

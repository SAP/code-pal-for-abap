CLASS ltc_if DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_if IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_empty_if_branches( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '     val = abap_true. ' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. "#EC EMPTY_IF_BRANCH' )
      ( '   ENDIF.' )
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
    result ?= NEW y_pal_empty_if_branches( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '     val = abap_true. ' )
      ( '   ELSEIF val = val. ' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '     val = abap_true. ' )
      ( '   ELSEIF val = val. ' )
      ( '     val = abap_false. ' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '     val = abap_true. ' )
      ( '   ELSEIF val = val. "#EC EMPTY_IF_BRANCH ' )
      ( '   ENDIF.' )
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
    result ?= NEW y_pal_empty_if_branches( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '     val = abap_true. ' )
      ( '   ELSE. ' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '     val = abap_true. ' )
      ( '   ELSE. ' )
      ( '     val = abap_false. ' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   DATA val TYPE abap_bool. ' )
      ( '   IF val = val. ' )
      ( '     val = abap_true. ' )
      ( '   ELSE.  "#EC EMPTY_IF_BRANCH ' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

ENDCLASS.

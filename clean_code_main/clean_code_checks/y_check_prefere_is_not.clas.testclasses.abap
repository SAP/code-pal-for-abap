CLASS ltc_not_is_initial DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_not_is_initial IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefere_is_not( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   IF NOT count IS INITIAL. ' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   IF count IS NOT INITIAL. ' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   IF NOT count IS INITIAL. "#EC PREFERE_IS_NOT ' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_not_contains_pattern DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_not_contains_pattern IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefere_is_not( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( |   DATA(text) = 'text'. | )
      ( '   DATA(count) = 0. ' )
      ( |   IF NOT text CP 't*'. | )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( |   DATA(text) = 'text'. | )
      ( '   DATA(count) = 0. ' )
      ( |   IF text NP 't*'. | )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.     ' )
      ( |   DATA(text) = 'text'. | )
      ( '   DATA(count) = 0. ' )
      ( |   IF NOT text CP 't*'. "#EC PREFERE_IS_NOT | )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_not_value DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_not_value IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefere_is_not( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( |   IF NOT count = 0. | )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( |   IF count <> 0. | )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( |   IF NOT count = 0. "#EC PREFERE_IS_NOT | )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

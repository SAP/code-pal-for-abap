CLASS ltc_not_is_initial DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_not_is_initial IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_is_not( ).
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
      ( '   IF NOT count IS INITIAL. "#EC PREFER_IS_NOT ' )
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
    result ?= NEW y_check_prefer_is_not( ).
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
      ( |   IF NOT text CP 't*'. "#EC PREFER_IS_NOT | )
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
    result ?= NEW y_check_prefer_is_not( ).
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
      ( |   IF NOT count = 0. "#EC PREFER_IS_NOT | )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_not_value_assert DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_not_value_assert IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( |   ASSERT NOT count = 0. | )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_not_value_check DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_not_value_check IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( |   CHECK NOT count = 0. | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_not_method DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_not_method IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_is_not( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS get_number RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD get_number. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->get_number( ) = 1.' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS get_number RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD get_number. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF object->get_number( ) <> 1.' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS get_number RETURNING VALUE(result) TYPE i. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD get_number. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->get_number( ) = 1. "#EC PREFER_IS_NOT' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_and DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_and IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_is_not( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.  ' )
      ( '   DATA(count) = 0.   ' )
      ( '   IF sy-subrc = 0    ' )
      ( '   AND NOT count = 1. ' )
      ( '     count = 1.       ' )
      ( '   ENDIF.             ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.  ' )
      ( '   DATA(count) = 0.   ' )
      ( '   IF sy-subrc = 0    ' )
      ( '   AND count <> 1.    ' )
      ( '     count = 1.       ' )
      ( '   ENDIF.             ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.                      ' )
      ( '   DATA(count) = 0.                       ' )
      ( '   IF sy-subrc = 0                        ' )
      ( '   AND NOT count = 1. "#EC PREFER_IS_NOT  ' )
      ( '     count = 1.                           ' )
      ( '   ENDIF.                                 ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_or DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_or IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_is_not( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.  ' )
      ( '   DATA(count) = 0.   ' )
      ( '   IF sy-subrc = 0    ' )
      ( '   OR NOT count = 1.  ' )
      ( '     count = 1.       ' )
      ( '   ENDIF.             ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.  ' )
      ( '   DATA(count) = 0.   ' )
      ( '   IF sy-subrc = 0    ' )
      ( '   OR count <> 1.     ' )
      ( '     count = 1.       ' )
      ( '   ENDIF.             ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.                     ' )
      ( '   DATA(count) = 0.                      ' )
      ( '   IF sy-subrc = 0                       ' )
      ( '   OR NOT count = 1. "#EC PREFER_IS_NOT  ' )
      ( '     count = 1.                          ' )
      ( '   ENDIF.                                ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_elseif DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_elseif IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_is_not( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(count) = 0. ' )
      ( '   IF sy-subrc = 0. ' )
      ( '     count = 1. ' )
      ( '   ELSEIF NOT count = 5. ' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.  ' )
      ( '   DATA(count) = 0. ' )
      ( '   IF sy-subrc = 0. ' )
      ( '     count = 1. ' )
      ( '   ELSEIF count <> 5. ' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )

      ( '   DATA(count) = 0. ' )
      ( '   IF sy-subrc = 0. ' )
      ( '     count = 1. ' )
      ( '   ELSEIF NOT count = 5. "#EC PREFER_IS_NOT ' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_line_exists DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_line_exists IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA itab TYPE TABLE OF tadir. ' )
      ( '   ASSERT NOT line_exists( itab[ 0 ] ). ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_contains DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_contains IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   ASSERT NOT contains( val = 'code pal for ABAP' sub = 'ABAP' ). | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_matches DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_matches IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   ASSERT NOT matches( val = 'a123e' regex = '[[:alpha:]]*' ). | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_contains_any_not_of DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_contains_any_not_of IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   ASSERT NOT contains_any_not_of( val = 'a123e' sub = '123' ). | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_contains_any_of DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_contains_any_of IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   ASSERT NOT contains_any_of( val = 'a123e' sub = '123' ). | )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_predicative_method DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_predicative_method IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS is_active RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD is_active. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->is_active( ).' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_predicative_with_param DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_predicative_with_param IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS is_active IMPORTING num TYPE i RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD is_active. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->is_active( 3 ).' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_two_predicative_methods DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_two_predicative_methods IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS is_active RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD is_active. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->is_active( ) AND NOT object->is_active( ).' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_predicative_nested DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_predicative_nested IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS is_active IMPORTING nested TYPE abap_bool RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD is_active. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->is_active( object->is_active( abap_true ) ).' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_new_predicative DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_new_predicative IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS is_active RETURNING VALUE(result) TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD is_active. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   IF NOT NEW y_example_class( )->is_active( ).' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_method_return_structure DEFINITION INHERITING FROM ltc_not_value FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_method_return_structure IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     TYPES: BEGIN OF ty_structure,' )
      ( '              comp TYPE i,' )
      ( '            END OF ty_structure.' )
      ( '     METHODS get_structure RETURNING VALUE(result) TYPE ty_structure. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD get_structure. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->get_structure( )-comp = 1.' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     TYPES: BEGIN OF ty_structure,' )
      ( '              comp TYPE i,' )
      ( '            END OF ty_structure.' )
      ( '     METHODS get_structure RETURNING VALUE(result) TYPE ty_structure. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD get_structure. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
      ( ' START-OF-SELECTION.      ' )
      ( '   DATA(count) = 0. ' )
      ( '   DATA(object) = NEW y_example_class( ). ' )
      ( '   IF NOT object->get_structure( )-comp = 1.  "#EC PREFER_IS_NOT' )
      ( '     count = 1. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.
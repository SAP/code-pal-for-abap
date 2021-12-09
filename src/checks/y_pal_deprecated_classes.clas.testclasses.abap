CLASS ltc_data DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_data IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_classes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA aunit TYPE REF TO cl_aunit_assert.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA aunit TYPE REF TO cl_abap_unit_assert.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA aunit TYPE REF TO cl_aunit_assert. "#EC DEPRECATED_CLAS' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_field_symbol DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_field_symbol IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_classes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   FIELD-SYMBOLS <aunit> TYPE REF TO cl_aunit_assert.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   FIELD-SYMBOLS <aunit> TYPE REF TO cl_abap_unit_assert.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   FIELD-SYMBOLS <aunit> TYPE REF TO cl_aunit_assert. "#EC DEPRECATED_CLAS' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_types DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_types IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_classes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   TYPES: BEGIN OF type,' )
      ( '            aunit TYPE REF TO cl_aunit_assert,' )
      ( '          END OF type.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   TYPES: BEGIN OF type,' )
      ( '            aunit TYPE REF TO cl_abap_unit_assert,' )
      ( '          END OF type.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   TYPES: BEGIN OF type,' )
      ( '            aunit TYPE REF TO cl_aunit_assert, "#EC DEPRECATED_CLAS' )
      ( '          END OF type.' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_static_attribute DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_static_attribute IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_classes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' INTERFACE example.' )
      ( '   CLASS-DATA aunit TYPE REF TO cl_aunit_assert.' )
      ( ' ENDINTERFACE.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' INTERFACE example.' )
      ( '   CLASS-DATA aunit TYPE REF TO cl_abap_unit_assert.' )
      ( ' ENDINTERFACE.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' INTERFACE example.' )
      ( '   CLASS-DATA aunit TYPE REF TO cl_aunit_assert. "#EC DEPRECATED_CLAS' )
      ( ' ENDINTERFACE.' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_instance_attribute DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_instance_attribute IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_classes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' CLASS example DEFINITION.' )
      ( '   PROTECTED SECTION.' )
      ( '     DATA aunit TYPE REF TO cl_aunit_assert.' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' CLASS example DEFINITION.' )
      ( '   PROTECTED SECTION.' )
      ( '     DATA aunit TYPE REF TO cl_abap_unit_assert.' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' CLASS example DEFINITION.' )
      ( '   PROTECTED SECTION.' )
      ( '     DATA aunit TYPE REF TO cl_aunit_assert. "#EC DEPRECATED_CLAS' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_parameter DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_parameter IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_classes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' CLASS example DEFINITION.' )
      ( '   PROTECTED SECTION.' )
      ( '     METHODS example IMPORTING aunit TYPE REF TO cl_aunit_assert.' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION.' )
      ( '   METHOD example.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' CLASS example DEFINITION.' )
      ( '   PROTECTED SECTION.' )
      ( '     METHODS example IMPORTING aunit TYPE REF TO cl_abap_unit_assert.' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION.' )
      ( '   METHOD example.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT test.' )

      ( ' CLASS example DEFINITION.' )
      ( '   PROTECTED SECTION.' )
      ( '     METHODS example IMPORTING aunit TYPE REF TO cl_aunit_assert. "#EC DEPRECATED_CLAS' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION.' )
      ( '   METHOD example.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_variable DEFINITION INHERITING FROM ltc_data FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_variable IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA cl_aunit_assert TYPE REF TO cl_abap_unit_assert.' )
    ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_multiple_times DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_times IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_classes( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 2.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA aunit TYPE REF TO cl_aunit_assert.' )
      ( '   DATA const TYPE REF TO if_aunit_constants.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA aunit TYPE REF TO cl_abap_unit_assert.' )
      ( '   DATA const TYPE REF TO if_abap_unit_constant.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA aunit TYPE REF TO cl_aunit_assert. "#EC DEPRECATED_CLAS' )
      ( '   DATA const TYPE REF TO if_aunit_constants. "#EC DEPRECATED_CLAS' )
    ).
  ENDMETHOD.

ENDCLASS.

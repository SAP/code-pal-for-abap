CLASS ltc_create_object DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_create_object IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_new_to_crt_obj( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA prefer_new_to_crt_obj TYPE REF TO y_check_prefer_new_to_crt_obj. ' )
      ( '     CREATE OBJECT prefer_new_to_crt_obj. ' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA(prefer_new_to_crt_obj) = NEW y_check_prefer_new_to_crt_obj( ). ' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA prefer_new_to_crt_obj TYPE REF TO y_check_prefer_new_to_crt_obj. ' )
      ( '     CREATE OBJECT prefer_new_to_crt_obj. "#EC PREF_NEW' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_dynamic_object_with_param DEFINITION INHERITING FROM ltc_create_object FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_dynamic_object_with_param IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA check TYPE REF TO y_check_base. | )
      ( |   DATA params TYPE abap_parmbind_tab. | )
      ( |   DATA(prefer_new) = 'y_check_prefer_new_to_crt_obj'. | )
      ( |   CREATE OBJECT check TYPE (prefer_new) PARAMETER-TABLE params. | )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_dynamic_object DEFINITION INHERITING FROM ltc_create_object FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_dynamic_object IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( |   DATA check TYPE REF TO y_check_base. | )
      ( |   DATA(prefer_new) = 'y_check_prefer_new_to_crt_obj'. | )
      ( |   CREATE OBJECT check TYPE (prefer_new). | )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_ole_calls DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_ole_calls IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example.' )

      ( 'START-OF-SELECTION.' )
      ( '  DATA excel TYPE ole2_object.' )
      ( '  CREATE OBJECT excel ''Excel.Application''.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    cl_aunit_assert=>abort( quit =  cl_aunit_assert=>method ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    cl_aunit_assert=>abort( quit =  cl_aunit_assert=>method ).
  ENDMETHOD.

  METHOD get_cut.
    result = NEW y_check_prefer_new_to_crt_obj( ).
  ENDMETHOD.

ENDCLASS.

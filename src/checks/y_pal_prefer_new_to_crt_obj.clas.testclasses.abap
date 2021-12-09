CLASS ltc_create_object DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_create_object IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_new_to_crt_obj( ).
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
      ( '     DATA prefer_new_to_crt_obj TYPE REF TO y_pal_prefer_new_to_crt_obj. ' )
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
      ( '     DATA(prefer_new_to_crt_obj) = NEW y_pal_prefer_new_to_crt_obj( ). ' )
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
      ( '     DATA prefer_new_to_crt_obj TYPE REF TO y_pal_prefer_new_to_crt_obj. ' )
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
      ( |   DATA check TYPE REF TO y_code_pal_base. | )
      ( |   DATA params TYPE abap_parmbind_tab. | )
      ( |   DATA(prefer_new) = 'y_pal_prefer_new_to_crt_obj'. | )
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
      ( |   DATA check TYPE REF TO y_code_pal_base. | )
      ( |   DATA(prefer_new) = 'y_pal_prefer_new_to_crt_obj'. | )
      ( |   CREATE OBJECT check TYPE (prefer_new). | )
    ).
  ENDMETHOD.

ENDCLASS.

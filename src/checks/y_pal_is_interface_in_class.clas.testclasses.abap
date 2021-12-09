CLASS ltc_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_is_interface_in_class( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT ut_repo.' )

      ( 'CLASS lcl_classname DEFINITION.' )
      ( ' PUBLIC SECTION.' )
      ( '  METHODS publ_method.' )
      ( ' PROTECTED SECTION.' )
      ( ' PRIVATE SECTION.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( ' METHOD publ_method.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT ut_repo.' )

      ( 'CLASS lcl_abstr DEFINITION ABSTRACT.' )
      ( ' PUBLIC SECTION.' )
      ( '  METHODS abstr_method ABSTRACT.' )
      ( '  METHODS constructor.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_abstr IMPLEMENTATION.' )
      ( ' METHOD constructor.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_inh_abstr DEFINITION INHERITING FROM lcl_abstr.' )
      ( ' PUBLIC SECTION.' )
      ( '  METHODS abstr_method REDEFINITION.' )
      ( '  METHODS constructor.' )
      ( ' PROTECTED SECTION.' )
      ( '  METHODS prot_method.' )
      ( ' PRIVATE SECTION.' )
      ( '  METHODS priv_method.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_inh_abstr IMPLEMENTATION.' )
      ( ' METHOD constructor.' )
      ( '  super->constructor( ).' )
      ( ' ENDMETHOD.' )

      ( ' METHOD abstr_method.' )
      ( ' ENDMETHOD.' )

      ( ' METHOD prot_method.' )
      ( ' ENDMETHOD.' )

      ( ' METHOD priv_method.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT ut_repo.' )

      ( 'CLASS lcl_classname DEFINITION. ' )
      ( ' PUBLIC SECTION. "#EC INTF_IN_CLASS' )
      ( '  METHODS publ_method.'  )
      ( ' PROTECTED SECTION.' )
      ( ' PRIVATE SECTION.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( ' METHOD publ_method.' )
      ( ' ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_test_class DEFINITION INHERITING FROM ltc_class FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_test_class IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_repo.' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.' )
      ( '   PUBLIC SECTION.' )
      ( '     METHODS publ_method.' )
      ( '     METHODS test_method FOR TESTING.' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION.' )
      ( '   METHOD publ_method.' )
      ( '   ENDMETHOD.' )
      ( '   METHOD test_method.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.

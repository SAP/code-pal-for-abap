CLASS ltc_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_is_interface_in_class( ).
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

      ( 'CLASS lcl_classname DEFINITION. "#EC INTF_IN_CLASS' )
      ( ' PUBLIC SECTION.' )
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

CLASS ltc_test_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_is_interface_in_class( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
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

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_repo.' )

      ( ' INTERFACE lif_classname. ' )
      ( '   METHODS publ_method. ' )
      ( ' ENDINTERFACE. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.' )
      ( '   PUBLIC SECTION.' )
      ( '     INTERFACES lif_classname.' )
      ( '     ALIASES publ_method FOR lif_classname~publ_method.' )
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

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT ut_repo.' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. "#EC INTF_IN_CLASS' )
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

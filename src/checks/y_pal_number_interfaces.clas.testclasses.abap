CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_number_interfaces( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     INTERFACES if_abap_c_reader. ' )
      ( '     INTERFACES if_abap_reader. ' )
      ( '     INTERFACES: ' )
      ( '       if_abap_c_writer, ' )
      ( '       if_abap_writer. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     INTERFACES if_abap_c_reader. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. "#EC NMBR_INTERFACES ' )
      ( '   PUBLIC SECTION. ' )
      ( '     INTERFACES if_abap_c_reader. ' )
      ( '     INTERFACES if_abap_reader. ' )
      ( '     INTERFACES: ' )
      ( '       if_abap_c_writer, ' )
      ( '       if_abap_writer. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

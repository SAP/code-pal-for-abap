CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_num_exec_statements( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA attribute_01 TYPE i. ' )
      ( '     DATA attribute_02 TYPE i. ' )
      ( '     DATA attribute_03 TYPE i. ' )
      ( '     DATA attribute_04 TYPE i. ' )
      ( '     DATA attribute_05 TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )

      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )

      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )

      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '     DATA(zero) = 0. ' )
      ( '     IF zero = 0. ' )
      ( '       "Do something... ' )
      ( '     ENDIF. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   DATA(zero) = 0. ' )
      ( '   IF zero = 0. ' )
      ( '     "Do something... ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS test. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA attribute_01 TYPE i. ' )
      ( '     DATA attribute_02 TYPE i. ' )
      ( '     DATA attribute_03 TYPE i. ' )
      ( '     DATA attribute_04 TYPE i. ' )
      ( '     DATA attribute_05 TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD test. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )

      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )

      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )

      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '    attribute_01 = attribute_02. ' )
      ( '    attribute_02 = attribute_03. ' )
      ( '    attribute_03 = attribute_04. ' )
      ( '    attribute_04 = attribute_05. ' )
      ( '    attribute_05 = attribute_01. ' )
      ( '   ENDMETHOD. "#EC CI_NOES ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

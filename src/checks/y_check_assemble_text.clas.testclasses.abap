CLASS ltc_ampersand_with_literal DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_ampersand_with_literal IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_assemble_text( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'This'. | )
      ( |   DATA(second) = 'is'. | )
      ( |   DATA(third) = 'an example'. | )

      ( |   WRITE first && ' - ' && second && ' ~ ' && third. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'This'. | )
      ( |   DATA(second) = 'is'. | )
      ( |   DATA(third) = 'an example'. | )

      ( '   WRITE | { first } { second } { third } |. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'This'. | )
      ( |   DATA(second) = 'is'. | )
      ( |   DATA(third) = 'an example'. | )

      ( |   WRITE first && ' - ' && second && ' ~ ' && third. "#EC ASSEMBLE_TEXT | )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_ampersand_without_literal DEFINITION INHERITING FROM ltc_ampersand_with_literal  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_ampersand_without_literal IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'This'. | )
      ( |   DATA(second) = 'is'. | )
      ( |   DATA(third) = 'an example'. | )

      ( |   WRITE first && second && third. | )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_concatenate DEFINITION INHERITING FROM ltc_ampersand_with_literal  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_concatenate IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'This'. | )
      ( |   DATA(second) = 'is'. | )
      ( |   DATA(third) = 'an example'. | )

      ( |   CONCATENATE first second third INTO DATA(result) SEPARATED BY space. | )
    ).
  ENDMETHOD.

ENDCLASS.

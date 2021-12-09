CLASS ltc_ampersand_with_literal DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_ampersand_with_literal IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_text_assembly( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'A'. | )
      ( |   DATA(second) = 'B'. | )
      ( |   DATA(third) = 'C'. | )

      ( |   WRITE first && ': ' && second && ' - ' && third. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'A'. | )
      ( |   DATA(second) = 'B'. | )
      ( |   DATA(third) = 'C'. | )

      ( '   WRITE |{ first }: { second } - { third }|. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   DATA(first) = 'A'. | )
      ( |   DATA(second) = 'B'. | )
      ( |   DATA(third) = 'C'. | )

      ( |   WRITE first && ': ' && second && ' - ' && third. "#EC TEXT_ASSEMBLY | )
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
      ( |   DATA(first) = 'A'. | )
      ( |   DATA(second) = 'B'. | )
      ( |   DATA(third) = 'C'. | )

      ( |   WRITE first && second && third. | )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_literals_only DEFINITION INHERITING FROM ltc_ampersand_with_literal  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_literals_only IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( |   WRITE '<p>' | )
      ( |      && '  <p>one</p>' | )
      ( |      && '  <p>two</p>' | )
      ( |      && '</p>'. | )
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
      ( |   DATA(first) = 'A'. | )
      ( |   DATA(second) = 'B'. | )
      ( |   DATA(third) = 'C'. | )

      ( |   CONCATENATE first second third INTO DATA(result) SEPARATED BY space. | )
    ).
  ENDMETHOD.

ENDCLASS.

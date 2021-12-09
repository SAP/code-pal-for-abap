CLASS ltc_variable DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_variable IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_message_easy_to_find( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 4.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   DATA(message_class) = '00'. | )
      ( |   DATA(message_id) = '002'. | )

      ( |   MESSAGE i002(message_class). | )
      ( |   MESSAGE i002(message_class) WITH 'Enter a valid value'. | )
      ( |   MESSAGE i002(message_class) WITH text-002. | )
      ( |   MESSAGE i002(message_class) WITH sy-uname. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE i002(00). | )
      ( |   MESSAGE i002(00) WITH 'Enter a valid value'(002). | )
      ( |   MESSAGE i002(00) WITH 'Enter a valid value'. | )
      ( |   MESSAGE i002(00) WITH text-003. | )
      ( |   MESSAGE i002(00) WITH sy-uname. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   DATA(message_class) = '00'. | )
      ( |   DATA(message_id) = '002'. | )

      ( |   MESSAGE i002(message_class). "#EC MSG_FIND | )
      ( |   MESSAGE i002(message_class) WITH 'Enter a valid value'. "#EC MSG_FIND | )
      ( |   MESSAGE i002(message_class) WITH text-002. "#EC MSG_FIND | )
      ( |   MESSAGE i002(message_class) WITH sy-uname. "#EC MSG_FIND | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_id_variable DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_id_variable IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_message_easy_to_find( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   DATA(message_class) = '00'. | )
      ( |   DATA(message_id) = '002'. | )

      ( |   MESSAGE ID message_class type 'I' NUMBER message_id. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE ID 00 type 'I' NUMBER 002. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   DATA(message_class) = '00'. | )
      ( |   DATA(message_id) = '002'. | )

      ( |   MESSAGE ID message_class type 'I' NUMBER message_id. "#EC MSG_FIND | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_hardcoded_id DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_hardcoded_id IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_message_easy_to_find( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE ID '00' type 'I' NUMBER '002'. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE i002(00). | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE ID '00' type 'I' NUMBER '002'. "#EC MSG_FIND | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_variable_named_message DEFINITION INHERITING FROM ltc_hardcoded_id FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_variable_named_message IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   DATA message TYPE sy-msgid. | )
      ( |   message = '00'. | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_object_with_message_class DEFINITION INHERITING FROM ltc_hardcoded_id FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_object_with_message_class IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example MESSAGE-ID 00. ' )
      ( ' START-OF-SELECTION. ' )
      ( '   MESSAGE i002. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_sy_msgid DEFINITION INHERITING FROM ltc_hardcoded_id FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_sy_msgid IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_try_catch DEFINITION INHERITING FROM ltc_hardcoded_id FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_try_catch IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )

      ( |   TRY. | )
      ( |       DATA(name) = 'codepal'. | )
      ( |     CATCH cx_sy_itab_line_not_found INTO DATA(line_not_found). | )
      ( |       DATA(text) = line_not_found->get_text( ). | )
      ( |       MESSAGE text TYPE 'W'. | )
      ( |   ENDTRY. | )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_hardcoded_msg DEFINITION INHERITING FROM ltc_hardcoded_id FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_hardcoded_msg IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE 'File not found' TYPE 'i'. | )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_text_elements DEFINITION INHERITING FROM ltc_hardcoded_id FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_text_elements IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE TEXT-006 TYPE 'I' DISPLAY LIKE 'S'. | )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_method DEFINITION INHERITING FROM ltc_hardcoded_id FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_method IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS lcl_classname DEFINITION.' )
      ( '   PUBLIC SECTION.' )
      ( '     METHODS raise.' )
      ( '   PROTECTED SECTION.' )
      ( '     CLASS-DATA error TYPE REF TO cx_root. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION.' )
      ( '   METHOD raise.' )
      ( '     MESSAGE error->get_text( ) TYPE sy-msgty. ' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.

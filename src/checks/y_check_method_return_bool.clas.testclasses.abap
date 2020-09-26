CLASS local_test_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_method_return_bool( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 11.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS active RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS entry RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS oficial RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS read RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS select RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS file RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS validate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS vowel RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS dot RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS concatenate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS value RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS sum IMPORTING a TYPE i b TYPE i RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS increment_one CHANGING integer TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD active. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD oficial. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD read. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD select. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD file. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD validate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD vowel. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD dot. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD concatenate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD value. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD sum. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD increment_one. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS is_active RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS has_entry RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS are_oficial RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS try_read RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS can_select RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS have_file RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS must_validate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS starts_vowel RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS ends_dot RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS should_concatenate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS exist_entry RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS contain_value RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS sum IMPORTING a TYPE i b TYPE i RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS increment_one CHANGING integer TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD is_active. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD has_entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD are_oficial. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD try_read. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD can_select. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD have_file. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD must_validate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD starts_vowel. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD ends_dot. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD should_concatenate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD exist_entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD contain_value. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD sum. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD increment_one. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS active RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS entry RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS oficial RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS read RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS select RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS file RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS validate RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS vowel RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS dot RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS concatenate RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS value RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS sum IMPORTING a TYPE i b TYPE i RETURNING VALUE(result) TYPE i. "#EC METH_RET_BOOL' )
      ( '     METHODS increment_one CHANGING integer TYPE i. "#EC METH_RET_BOOL' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD active. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD oficial. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD read. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD select. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD file. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD validate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD vowel. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD dot. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD concatenate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD value. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD sum. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD increment_one. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

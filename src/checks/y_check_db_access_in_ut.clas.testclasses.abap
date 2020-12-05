CLASS ltc_select DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_select IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA tadir TYPE tadir. ' )
      ( '     SELECT SINGLE * FROM tadir INTO tadir. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '    DATA(tadir) = VALUE tt_tadir( ).' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA tadir TYPE tadir. ' )
      ( '     SELECT SINGLE * FROM tadir INTO tadir. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_delete DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_delete IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA(line) = VALUE tadir( obj_name = 'xyz' ). | )
      ( '     DELETE tadir FROM line. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     "No db use ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( |     DATA(line) = VALUE tadir( obj_name = 'xyz' ). | )
      ( '     DELETE tadir FROM line. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_modify DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_modify IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(line) = VALUE tadir( ). ' )
      ( '     MODIFY tadir FROM line. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     "No db use ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(line) = VALUE tadir( ). ' )
      ( '     MODIFY tadir FROM line. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_update DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_update IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(line) = VALUE tadir( ). ' )
      ( '     UPDATE tadir FROM line. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     "No db use ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(line) = VALUE tadir( ). ' )
      ( '     UPDATE tadir FROM line. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_insert DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_insert IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(line) = VALUE tadir( ). ' )
      ( '     INSERT tadir FROM line. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     "No db use ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA(line) = VALUE tadir( ). ' )
      ( '     INSERT tadir FROM line. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_rollback DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_rollback IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     ROLLBACK WORK. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     "No db use ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     ROLLBACK WORK. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_commit DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_commit IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     COMMIT WORK. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     "No db use ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     COMMIT WORK AND WAIT. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_osql_framework DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_osql_framework IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA tadir TYPE tadir. ' )
      ( '     SELECT SINGLE * FROM tadir INTO tadir. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS scenario FOR TESTING. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS given_fake_value. ' )
      ( '     METHODS when_select. ' )
      ( '     METHODS then_has_entry. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-DATA osql_test_environment TYPE REF TO if_osql_test_environment. ' )
      ( '     DATA cut TYPE tadir. ' )
      ( '     CLASS-METHODS class_setup. ' )
      ( '     CLASS-METHODS class_teardown. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )

      ( '   METHOD class_setup. ' )
      ( |     osql_test_environment = cl_osql_test_environment=>create( VALUE #( ( 'tadir' ) ) ). | )
      ( '   ENDMETHOD. ' )

      ( '   METHOD class_teardown. ' )
      ( '     osql_test_environment->destroy( ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD given_fake_value. ' )
      ( '     "osql_test_environment->insert_test_data( data ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD when_select. ' )
      ( '     SELECT SINGLE * FROM tadir INTO cut. ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD then_has_entry. ' )
      ( '     cl_abap_unit_assert=>assert_not_initial( cut ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD scenario. ' )
      ( '     given_fake_value( ). ' )
      ( '     when_select( ). ' )
      ( '     then_has_entry( ). ' )
      ( '   ENDMETHOD. ' )

      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_classname IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA tadir TYPE tadir. ' )
      ( '     SELECT SINGLE * FROM tadir INTO tadir. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

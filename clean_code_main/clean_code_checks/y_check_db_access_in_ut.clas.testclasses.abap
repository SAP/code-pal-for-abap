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

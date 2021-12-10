CLASS ltc_osql_framework DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_osql_framework IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_db_access_in_ut( ).
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



CLASS ltc_osql_framework_inherited DEFINITION INHERITING FROM ltc_osql_framework FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_osql_framework_inherited IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test. ' )

      ( ' CLASS lcl_base DEFINITION FOR TESTING. ' )
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

      ( ' CLASS lcl_base IMPLEMENTATION. ' )

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

      ( ' ENDCLASS. ' )

      ( ' CLASS lcl_test DEFINITION FOR TESTING INHERITING FROM lcl_base. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS scenario FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS lcl_test IMPLEMENTATION. ' )
      ( '   METHOD scenario. ' )
      ( '     DATA entries TYPE tadir. ' )
      ( '     given_fake_value( ). ' )
      ( '     SELECT SINGLE * FROM tadir INTO entries. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_risk_harmless DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_risk_harmless IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 8.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA profile TYPE ytab_profiles. ' )

      ( '     SELECT SINGLE * FROM ytab_profiles INTO profile. ' )

      ( '     INSERT INTO ytab_profiles VALUES profile. ' )
      ( '     MODIFY ytab_profiles FROM profile. ' )
      ( '     UPDATE ytab_profiles FROM profile. ' )
      ( '     DELETE ytab_profiles FROM profile. ' )

      ( '     COMMIT WORK. ' )
      ( '     COMMIT WORK AND WAIT. ' )

      ( '     ROLLBACK WORK. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA profiles TYPE TABLE OF ytab_profiles. ' )

      ( '     DATA(profile) = VALUE ytab_profiles( profile = sy-uname username = sy-uname ). ' )

      ( '     INSERT profile INTO TABLE profiles. ' )
      ( '     MODIFY profiles FROM profile INDEX 1. ' )
      ( '     DELETE profiles FROM profile. ' )
      ( '     DELETE profiles INDEX lines( profiles ). ' )

      ( '     MODIFY profiles FROM VALUE #( ) TRANSPORTING profile WHERE username = sy-uname. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA profile TYPE ytab_profiles. ' )

      ( '     SELECT SINGLE * FROM ytab_profiles INTO profile. "#EC DB_ACCESS_UT ' )

      ( '     INSERT INTO ytab_profiles VALUES profile. "#EC DB_ACCESS_UT ' )
      ( '     MODIFY ytab_profiles FROM profile. "#EC DB_ACCESS_UT ' )
      ( '     UPDATE ytab_profiles FROM profile. "#EC DB_ACCESS_UT ' )
      ( '     DELETE ytab_profiles FROM profile. "#EC DB_ACCESS_UT ' )

      ( '     COMMIT WORK. "#EC DB_ACCESS_UT ' )
      ( '     COMMIT WORK AND WAIT. "#EC DB_ACCESS_UT ' )

      ( '     ROLLBACK WORK. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_risk_critical DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_risk_critical IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 3.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA profile TYPE ytab_profiles. ' )
      ( '     MODIFY ytab_profiles FROM profile. ' )
      ( '     UPDATE ytab_profiles FROM profile. ' )
      ( '     DELETE FROM ytab_profiles WHERE username = sy-uname. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA profile TYPE ytab_profiles. ' )

      ( '     SELECT SINGLE * FROM ytab_profiles INTO profile. ' )

      ( '     INSERT INTO ytab_profiles VALUES profile. ' )

      ( '     COMMIT WORK. ' )
      ( '     COMMIT WORK AND WAIT. ' )

      ( '     ROLLBACK WORK. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA profile TYPE ytab_profiles. ' )
      ( '     MODIFY ytab_profiles FROM profile. "#EC DB_ACCESS_UT ' )
      ( '     UPDATE ytab_profiles FROM profile. "#EC DB_ACCESS_UT ' )
      ( '     DELETE FROM ytab_profiles WHERE username = sy-uname. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_risk_dangerous DEFINITION INHERITING FROM ltc_risk_critical FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
  PRIVATE SECTION.
    METHODS convert_critical_to_dangerous IMPORTING code          TYPE y_code_pal_ref_scan_double=>source_code
                                          RETURNING VALUE(result) TYPE y_code_pal_ref_scan_double=>source_code.
ENDCLASS.

CLASS ltc_risk_dangerous IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = convert_critical_to_dangerous( super->get_code_with_issue( ) ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = convert_critical_to_dangerous( super->get_code_without_issue( ) ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = convert_critical_to_dangerous( super->get_code_with_exemption( ) ).
  ENDMETHOD.

  METHOD convert_critical_to_dangerous.
    TRY.
        DATA(index) = line_index( code[ table_line = ' CLASS example DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION SHORT. ' ] ).
        result = code.
        result[ index ] = ' CLASS example DEFINITION FOR TESTING RISK LEVEL DANGEROUS DURATION SHORT. '.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.



CLASS ltc_risk_not_set DEFINITION INHERITING FROM ltc_risk_harmless FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
  PRIVATE SECTION.
    METHODS convert_harmless_to_not_set IMPORTING code          TYPE y_code_pal_ref_scan_double=>source_code
                                        RETURNING VALUE(result) TYPE y_code_pal_ref_scan_double=>source_code.
ENDCLASS.

CLASS ltc_risk_not_set IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = convert_harmless_to_not_set( super->get_code_with_issue( ) ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = convert_harmless_to_not_set( super->get_code_without_issue( ) ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = convert_harmless_to_not_set( super->get_code_with_exemption( ) ).
  ENDMETHOD.

  METHOD convert_harmless_to_not_set.
    TRY.
        DATA(index) = line_index( code[ table_line = ' CLASS example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' ] ).
        result = code.
        result[ index ] = ' CLASS example DEFINITION FOR TESTING. '.
    CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.



CLASS ltc_exec_sql DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_exec_sql IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_db_access_in_ut( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     EXEC SQL. ' )
      ( '       DELETE FROM ytab_profiles WHERE username = sy-uname; ' )
      ( '     ENDEXEC. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '      ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DELETE FROM ytab_profiles WHERE username = sy-uname. "#EC DB_ACCESS_UT ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_itab DEFINITION INHERITING FROM ltc_risk_harmless FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_itab IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT unit_test. ' )

      ( ' CLASS example DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example FOR TESTING. ' )
      ( ' ENDCLASS.' )

      ( ' CLASS example IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA mt_messages TYPE symsg_tab. ' )
      ( '     DATA message TYPE symsg. ' )
      ( '     INSERT message INTO TABLE mt_messages. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

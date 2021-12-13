CLASS y_code_pal_profile DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_profile.
    ALIASES create FOR y_if_code_pal_profile~create.
    ALIASES get_checks_from_db FOR y_if_code_pal_profile~get_checks_from_db.
    ALIASES types FOR y_if_code_pal_profile~types.
    ALIASES mass_change FOR y_if_code_pal_profile~mass_change.

  PROTECTED SECTION.
    METHODS has_time_collision
      IMPORTING timeline_one_start TYPE dats
                timeline_one_end   TYPE dats
                timeline_two_start TYPE dats
                timeline_two_end   TYPE dats
      RETURNING VALUE(result)      TYPE abap_bool.

    METHODS is_point_in_time
      IMPORTING time_start    TYPE dats
                time_end      TYPE dats
                point         TYPE dats
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS standardprofile TYPE ytab_profiles-profile VALUE 'SYSTEM-WIDE STANDARD'.
    CLASS-METHODS get_check_base_package RETURNING VALUE(result) TYPE devclass.
    CLASS-METHODS get_checks_package RETURNING VALUE(result) TYPE devclass.

ENDCLASS.



CLASS y_code_pal_profile IMPLEMENTATION.


  METHOD has_time_collision.
    result = xsdbool( is_point_in_time( time_start = timeline_one_start
                                        time_end = timeline_one_end
                                        point = timeline_two_start ) OR

                      is_point_in_time( time_start = timeline_one_start
                                        time_end = timeline_one_end
                                        point = timeline_two_end ) OR

                      is_point_in_time( time_start = timeline_two_start
                                        time_end = timeline_two_end
                                        point = timeline_one_start ) OR

                      is_point_in_time( time_start = timeline_two_start
                                        time_end = timeline_two_end
                                        point = timeline_one_end ) ).
  ENDMETHOD.


  METHOD is_point_in_time.
    result = xsdbool( time_start <= point AND time_end >= point ).
  ENDMETHOD.


  METHOD y_if_code_pal_profile~check_delegation_rights.
    SELECT SINGLE delegate
    FROM ytab_delegates
    INTO @DATA(delegate)
    WHERE profile = @profile
    AND delegate = @sy-uname.

    IF delegate IS INITIAL.
      RAISE EXCEPTION TYPE ycx_code_pal_delegation_rights.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~check_time_overlap.
    DATA table TYPE SORTED TABLE OF ytab_checks WITH UNIQUE KEY profile
                                                                checkid
                                                                start_date
                                                                end_date
                                                                objects_created_on
                                                                prio
                                                                apply_on_testcode
                                                                ignore_pseudo_comments
                                                                evaluate_new_child_objects.

    IF check-start_date > check-end_date.
      RAISE EXCEPTION TYPE ycx_code_pal_time_overlap.
    ENDIF.

    SELECT * FROM ytab_checks INTO TABLE @table WHERE profile = @check-profile AND
                                                      checkid = @check-checkid AND
                                                      prio = @check-prio AND
                                                      objects_created_on = @check-objects_created_on AND
                                                      apply_on_testcode = @check-apply_on_testcode AND
                                                      ignore_pseudo_comments = @check-ignore_pseudo_comments AND
                                                      evaluate_new_child_objects = @check-evaluate_new_child_objects.

    IF sy-subrc = 0.
      IF selected_check IS NOT INITIAL.
        DELETE TABLE table WITH TABLE KEY profile = selected_check-profile
                                          checkid = selected_check-checkid
                                          start_date = selected_check-start_date
                                          end_date = selected_check-end_date
                                          objects_created_on = selected_check-objects_created_on
                                          prio = selected_check-prio
                                          apply_on_testcode = selected_check-apply_on_testcode
                                          ignore_pseudo_comments = selected_check-ignore_pseudo_comments
                                          evaluate_new_child_objects = selected_check-evaluate_new_child_objects.
      ENDIF.

      LOOP AT table INTO DATA(line).
        IF has_time_collision( timeline_one_start = line-start_date
                               timeline_one_end   = line-end_date
                               timeline_two_start = check-start_date
                               timeline_two_end   = check-end_date ) = abap_true.
          RAISE EXCEPTION TYPE ycx_code_pal_time_overlap.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~delete_check.
    DELETE FROM ytab_checks WHERE profile = @check-profile AND
                                  checkid = @check-checkid AND
                                  start_date = @check-start_date AND
                                  end_date = @check-end_date AND
                                  objects_created_on = @check-objects_created_on AND
                                  prio = @check-prio AND
                                  threshold = @check-threshold AND
                                  apply_on_testcode = @check-apply_on_testcode AND
                                  ignore_pseudo_comments = @check-ignore_pseudo_comments AND
                                  evaluate_new_child_objects = @check-evaluate_new_child_objects.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_remove_a_line.
    ENDIF.
    COMMIT WORK.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~delete_delegate.
    DELETE FROM ytab_delegates WHERE profile = @delegate-profile AND
                                     delegate = @delegate-delegate.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_remove_a_line.
    ENDIF.
    COMMIT WORK.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~delete_profile.
    IF profile-is_standard = abap_true.
      RAISE EXCEPTION TYPE ycx_code_pal_remove_a_line.
    ENDIF.

    DELETE FROM ytab_profiles WHERE username = @profile-username AND
                                    profile = @profile-profile AND
                                    is_standard = @abap_false.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_remove_a_line.
    ENDIF.

    TRY.
        y_if_code_pal_profile~select_checks( profile-profile ).
      CATCH ycx_code_pal_entry_not_found.
        DELETE FROM ytab_delegates WHERE profile = @profile-profile.
    ENDTRY.
    COMMIT WORK.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~delete_profiles.
    TRY.
        DATA(profiles) = y_if_code_pal_profile~select_profiles( sy-uname ).
      CATCH ycx_code_pal_entry_not_found.
        RETURN.
    ENDTRY.

    LOOP AT profiles ASSIGNING FIELD-SYMBOL(<profile>).
      y_if_code_pal_profile~delete_profile( <profile> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~get_check_description.
    SELECT SINGLE descript FROM vseoclass WHERE langu = @sy-langu AND clsname = @classname INTO @result.
    IF sy-subrc <> 0.
      SELECT SINGLE descript FROM vseoclass WHERE clsname = @classname INTO @result.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~get_registered_profiles.
    SELECT profile FROM ytab_checks APPENDING TABLE @result.
    DATA(prof_subrc) = sy-subrc.

    SELECT profile FROM ytab_profiles APPENDING TABLE @result.
    IF sy-subrc <> 0 AND
       prof_subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
    ENDIF.

    SORT result AS TEXT ASCENDING.
    DELETE ADJACENT DUPLICATES FROM result.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~import_profile.
    DATA(profile) = structure-profile.
    DATA(delegates) = structure-delegates.
    DATA(checks) = structure-checks.

    IF y_if_code_pal_profile~profile_exists( profile-profile ).
      y_if_code_pal_profile~check_delegation_rights( profile-profile ).
    ENDIF.

    profile-last_changed_by = sy-uname.
    profile-last_changed_on = sy-datum.
    profile-last_changed_at = sy-timlo.

    y_if_code_pal_profile~insert_profile( profile ).

    y_if_code_pal_profile~cleanup_profile( profile-profile ).

    LOOP AT delegates ASSIGNING FIELD-SYMBOL(<delegate>).
      y_if_code_pal_profile~insert_delegate( <delegate> ).
    ENDLOOP.

    LOOP AT checks ASSIGNING FIELD-SYMBOL(<check>).
      <check>-last_changed_by = sy-uname.
      <check>-last_changed_on = sy-datum.
      <check>-last_changed_at = sy-timlo.

      y_if_code_pal_profile~insert_check( <check> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~insert_check.
    INSERT INTO ytab_checks VALUES @check.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_add_a_line.
    ENDIF.
    COMMIT WORK.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~insert_delegate.
    INSERT INTO ytab_delegates VALUES @delegate.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_add_a_line.
    ENDIF.
    COMMIT WORK.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~insert_profile.
    IF profile-is_standard = abap_true OR
       profile-profile = standardprofile.
      RAISE EXCEPTION TYPE ycx_code_pal_add_a_line.
    ENDIF.
    MODIFY ytab_profiles FROM @profile.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_add_a_line.
    ENDIF.
    COMMIT WORK AND WAIT.

    TRY.
        y_if_code_pal_profile~select_delegates( profile-profile ).
      CATCH ycx_code_pal_entry_not_found.
        y_if_code_pal_profile~insert_delegate( VALUE ytab_delegates( profile = profile-profile
                                                                    delegate = sy-uname ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~register_standard_profile.
    SELECT SINGLE @abap_true
    FROM ytab_profiles
    INTO @DATA(exists)
    WHERE is_standard = @abap_true.

    IF exists = abap_false.
      DATA(profile) = VALUE ytab_profiles( username = 'ADMIN'
                                           profile = standardprofile
                                           is_standard = abap_true
                                           last_changed_by = 'ADMIN'
                                           last_changed_on = sy-datlo
                                           last_changed_at = sy-timlo ). "#EC DECL_IN_IF

      INSERT INTO ytab_profiles VALUES @profile.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_failed.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~select_checks.
    SELECT * FROM ytab_checks INTO TABLE @result WHERE profile = @profile.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~select_delegates.
    SELECT * FROM ytab_delegates INTO TABLE @result WHERE profile = @profile.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~select_existing_checks.
    DATA(checks) = get_checks_from_db( ).

    IF checks IS INITIAL.
      RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
    ENDIF.

    LOOP AT checks ASSIGNING FIELD-SYMBOL(<line>).

      SELECT SINGLE clsname, descript
      FROM vseoclass
      INTO @DATA(line)
      WHERE ( langu = @sy-langu OR langu = 'E' )
      AND clsname = @<line>-obj_name.

      IF sy-subrc = 0.
        APPEND line TO result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD y_if_code_pal_profile~select_profiles.
    SELECT * FROM ytab_profiles INTO TABLE @result WHERE username = @username OR
                                                         is_standard = @abap_true.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
    ENDIF.

    LOOP AT result ASSIGNING FIELD-SYMBOL(<line>) WHERE username <> username AND is_standard = abap_true.
      <line>-username = username.
    ENDLOOP.
    UNASSIGN <line>.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~select_all_profiles.
    "Based on Checks because the profile might be inactive
    SELECT DISTINCT profile FROM ytab_checks INTO TABLE @DATA(profiles).
    LOOP AT profiles ASSIGNING FIELD-SYMBOL(<profile>).
      APPEND VALUE ytab_profiles( profile = <profile> ) TO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~cleanup_profile.
    y_if_code_pal_profile~remove_all_delegates( profile ).
    y_if_code_pal_profile~remove_all_checks( profile ).
  ENDMETHOD.


  METHOD y_if_code_pal_profile~remove_all_delegates.
    TRY.
        DATA(delegates) = y_if_code_pal_profile~select_delegates( profile ).
      CATCH ycx_code_pal_entry_not_found.
        RETURN.
    ENDTRY.
    LOOP AT delegates ASSIGNING FIELD-SYMBOL(<delegate>).
      TRY.
          y_if_code_pal_profile~delete_delegate( <delegate> ).
        CATCH ycx_code_pal_remove_a_line.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~remove_all_checks.
    TRY.
        DATA(checks) = y_if_code_pal_profile~select_checks( profile ).
      CATCH ycx_code_pal_entry_not_found.
        RETURN.
    ENDTRY.
    LOOP AT checks ASSIGNING FIELD-SYMBOL(<check>).
      TRY.
          y_if_code_pal_profile~delete_check( <check> ).
        CATCH ycx_code_pal_remove_a_line.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~profile_exists.
    TRY.
        result = xsdbool( y_if_code_pal_profile~select_delegates( name ) IS NOT INITIAL ).
      CATCH ycx_code_pal_entry_not_found.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD get_checks_package.
    result = get_check_base_package( ).
    REPLACE 'FOUNDATION' IN result WITH 'CHECKS'.
  ENDMETHOD.


  METHOD get_checks_from_db.
    DATA(package) = get_checks_package( ).

    SELECT *
    FROM tadir
    WHERE devclass = @package
      AND NOT obj_name = @package
    INTO TABLE @result.
  ENDMETHOD.


  METHOD get_check_base_package.
    SELECT SINGLE devclass
    FROM tadir
    INTO @result
    WHERE obj_name = 'Y_CODE_PAL_BASE'.
  ENDMETHOD.


  METHOD y_if_code_pal_profile~create.
    result = NEW y_code_pal_profile( ).
  ENDMETHOD.


  METHOD mass_change.
    DATA check TYPE REF TO y_code_pal_base.
    TRY.
        DATA(checks) = y_if_code_pal_profile~select_checks( name ).

        LOOP AT checks INTO DATA(temp_config).
          TRY.
              CREATE OBJECT check TYPE (temp_config-checkid).
            CATCH cx_sy_create_object_error.
              CONTINUE.
          ENDTRY.

          DATA(temp_check) = temp_config.

          IF change_validation_period = abap_true.
            temp_check-start_date = config-start_date.
            temp_check-end_date = config-end_date.
          ENDIF.

          IF change_created_since = abap_true.
            temp_check-objects_created_on = config-objects_created_on.
          ENDIF.

          IF change_prio = abap_true AND config-prio <> space.
            temp_check-prio = config-prio.
          ENDIF.

          IF change_apply_prod_code = abap_true AND check->settings-disable_on_prodcode_selection = abap_false.
            temp_check-apply_on_productive_code = config-apply_on_productive_code.
          ENDIF.

          IF change_apply_testcode = abap_true AND check->settings-disable_on_testcode_selection = abap_false.
            temp_check-apply_on_testcode = config-apply_on_testcode.
          ENDIF.

          IF change_allow_exemptios = abap_true AND check->settings-pseudo_comment <> space.
            temp_check-ignore_pseudo_comments = config-ignore_pseudo_comments.
          ENDIF.

          IF change_evaluate_new_child_obj = abap_true.
            temp_check-evaluate_new_child_objects = config-evaluate_new_child_objects.
          ENDIF.

          TRY.
              y_if_code_pal_profile~delete_check( temp_config ).
              y_if_code_pal_profile~insert_check( temp_check ).

            CATCH ycx_code_pal_remove_a_line.

            CATCH ycx_code_pal_add_a_line
                  ycx_code_pal_time_overlap.

              TRY.
                  y_if_code_pal_profile~insert_check( temp_config ).
                CATCH ycx_code_pal_add_a_line
                      ycx_code_pal_time_overlap.
              ENDTRY.

          ENDTRY.
        ENDLOOP.

      CATCH ycx_code_pal_entry_not_found.
        RAISE EXCEPTION TYPE cx_failed.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

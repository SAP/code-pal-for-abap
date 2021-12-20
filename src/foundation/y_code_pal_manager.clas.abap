CLASS y_code_pal_manager DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_manager.
    METHODS constructor IMPORTING srcid TYPE scr_source_id.

  PROTECTED SECTION.
    ALIASES database_access FOR y_if_code_pal_manager~database_access.
    ALIASES creation_date FOR y_if_code_pal_manager~creation_date.
    ALIASES exemption FOR y_if_code_pal_manager~exemption.
    ALIASES statistics FOR y_if_code_pal_manager~statistics.
    ALIASES scope FOR y_if_code_pal_manager~scope.
    ALIASES profile FOR y_if_code_pal_manager~profile.

  PRIVATE SECTION.
    METHODS get_profiles RETURNING VALUE(result) TYPE string_table
                         RAISING   ycx_code_pal_no_customizing.

    METHODS get_profile_checks IMPORTING name  TYPE ycicc_profile
                                         checkid TYPE seoclsname
                             RETURNING VALUE(result) TYPE y_if_code_pal_manager=>check_configurations
                             RAISING   ycx_code_pal_no_customizing.

ENDCLASS.



CLASS y_code_pal_manager IMPLEMENTATION.

  METHOD constructor.
    database_access = NEW y_code_pal_database_access( srcid ).
    creation_date = NEW y_code_pal_creation_date( database_access ).
    exemption = NEW y_code_pal_exemption( database_access ).
    profile = NEW y_code_pal_profile( ).
  ENDMETHOD.


  METHOD y_if_code_pal_manager~get_profile_configuration.
    LOOP AT get_profiles( ) ASSIGNING FIELD-SYMBOL(<profile>).
      TRY.
          APPEND LINES OF get_profile_checks( name = CONV #( <profile> )
                                              checkid = checkid ) TO result.
        CATCH ycx_code_pal_no_customizing.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    IF lines( result ) = 0.
      RAISE EXCEPTION TYPE ycx_code_pal_no_customizing.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_code_pal_manager~set_scope.
    scope = NEW y_code_pal_scope( database_access = database_access
                                  leading_include = include ).
  ENDMETHOD.


  METHOD get_profiles.
    TRY.
        DATA(profiles) = profile->select_profiles( sy-uname ).
      CATCH ycx_code_pal_entry_not_found.
        RAISE EXCEPTION TYPE ycx_code_pal_no_customizing.
    ENDTRY.

    LOOP AT profiles ASSIGNING FIELD-SYMBOL(<profile>).
      APPEND <profile>-profile TO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_profile_checks.
    TRY.
        DATA(checks) = profile->select_checks( name ).
      CATCH ycx_code_pal_entry_not_found.
        RETURN.
    ENDTRY.

    LOOP AT checks ASSIGNING FIELD-SYMBOL(<check>)
    WHERE checkid = checkid
    AND start_date <= sy-datlo
    AND end_date >= sy-datlo.
      DATA(check_configuration) = VALUE y_if_code_pal_manager=>check_configuration( object_creation_date = <check>-objects_created_on
                                                                                    threshold = <check>-threshold
                                                                                    prio = <check>-prio
                                                                                    apply_on_productive_code = <check>-apply_on_productive_code
                                                                                    apply_on_testcode = <check>-apply_on_testcode
                                                                                    ignore_pseudo_comments = <check>-ignore_pseudo_comments
                                                                                    evaluate_new_child_objects = <check>-evaluate_new_child_objects ).
      result = VALUE #( BASE result ( CORRESPONDING #( check_configuration ) ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS y_code_pal_telemetry DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_telemetry.

  PROTECTED SECTION.
    ALIASES events FOR y_if_code_pal_telemetry~events.

  PRIVATE SECTION.
    CONSTANTS active VALUE abap_true.

    METHODS add IMPORTING entry TYPE ycodepaltlmy.
    METHODS get_timestamp RETURNING VALUE(result) TYPE ycodepaltlmy-timestamp.
ENDCLASS.



CLASS y_code_pal_telemetry IMPLEMENTATION.

  METHOD y_if_code_pal_telemetry~add_usage.
    CHECK active = abap_true.

    add( VALUE ycodepaltlmy( event = events-usage
                                check_name = check
                                timestamp = get_timestamp( ) ) ).
  ENDMETHOD.


  METHOD y_if_code_pal_telemetry~add_finding.
    CHECK active = abap_true.

    add( VALUE ycodepaltlmy( event = events-finding
                                check_name = check
                                timestamp = get_timestamp( ) ) ).
  ENDMETHOD.


  METHOD y_if_code_pal_telemetry~get_count.
    SELECT COUNT( * )
    FROM ycodepaltlmy
    INTO @result
    WHERE event = @event
    AND check_name = @check.
  ENDMETHOD.


  METHOD y_if_code_pal_telemetry~get_counters.
    SELECT event, check_name, COUNT( * ) AS count
    FROM ycodepaltlmy
    INTO TABLE @result
    GROUP BY event, check_name.
  ENDMETHOD.


  METHOD add.
    DATA table TYPE TABLE OF ycodepaltlmy.
    APPEND entry TO table.
    INSERT ycodepaltlmy FROM TABLE @table.
  ENDMETHOD.


  METHOD get_timestamp.
    GET TIME STAMP FIELD result.
  ENDMETHOD.

ENDCLASS.

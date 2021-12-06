INTERFACE y_if_code_pal_telemetry PUBLIC.

  TYPES event TYPE i.

  TYPES: BEGIN OF counter,
           event TYPE event,
           check TYPE sci_chk,
           count TYPE i,
         END OF counter.

  TYPES counters TYPE TABLE OF counter WITH DEFAULT KEY.

  CONSTANTS: BEGIN OF events,
               usage TYPE event VALUE 1,
               finding TYPE event VALUE 2,
             END OF events.

  METHODS add_usage IMPORTING check TYPE counter-check.
  METHODS add_finding IMPORTING check TYPE counter-check.

  METHODS get_count IMPORTING event TYPE counter-event
                              check TYPE counter-check
                    RETURNING VALUE(result) TYPE counter-count.

  METHODS get_counters RETURNING VALUE(result) TYPE counters.

ENDINTERFACE.

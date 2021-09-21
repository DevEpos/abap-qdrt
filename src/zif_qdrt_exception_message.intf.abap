"! <p class="shorttext synchronized" lang="en">Exception message</p>
INTERFACE zif_qdrt_exception_message
  PUBLIC.

  DATA:
    "! <p class="shorttext synchronized" lang="en">HTTP status for response</p>
    http_status TYPE i READ-ONLY,
    callstack   TYPE abap_callstack READ-ONLY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves message from exception</p>
    get_message
      RETURNING
        VALUE(result) TYPE string.
ENDINTERFACE.

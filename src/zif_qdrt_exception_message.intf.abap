"! <p class="shorttext synchronized" lang="en">Exception message</p>
INTERFACE zif_qdrt_exception_message
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Prints exception</p>
    print
      IMPORTING
        !iv_msg_type      TYPE sy-msgty DEFAULT 'S'
        !iv_display_type  TYPE sy-msgty DEFAULT 'E'
        !if_to_screen     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_message) TYPE string,
    "! <p class="shorttext synchronized" lang="en">Retrieves message from exception</p>
    get_message
      RETURNING
        VALUE(result) TYPE string.
ENDINTERFACE.

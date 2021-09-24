"! <p class="shorttext synchronized" lang="en">Utilty for REST request handling</p>
CLASS zcl_qdrt_rest_request_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Checks if uri attribute was supplied</p>
      check_empty_uri_attribute
        IMPORTING
          uri_attribute TYPE string
          value         TYPE any
        RAISING
          zcx_qdrt_appl_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_rest_request_util IMPLEMENTATION.


  METHOD check_empty_uri_attribute.
    CHECK value IS INITIAL.

    RAISE EXCEPTION TYPE zcx_qdrt_appl_error
      EXPORTING
        status = cl_rest_status_code=>gc_client_error_bad_request
        text   = |URI attribute '{ uri_attribute }' is missing|.
  ENDMETHOD.


ENDCLASS.

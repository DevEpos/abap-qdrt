"! <p class="shorttext synchronized" lang="en">Utility for REST response</p>
CLASS zcl_qdrt_rest_error_response DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      create
        IMPORTING
          response      TYPE REF TO if_rest_response
        RETURNING
          VALUE(result) TYPE REF TO zcl_qdrt_rest_error_response.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Sets error in response for appl exception</p>
      set_body_from_exc
        IMPORTING
          error         TYPE REF TO zif_qdrt_exception_message
        RETURNING
          VALUE(result) TYPE REF TO zcl_qdrt_rest_error_response,
      "! <p class="shorttext synchronized" lang="en">text</p>
      set_body_from_text
        IMPORTING
          text          TYPE string
        RETURNING
          VALUE(result) TYPE REF TO zcl_qdrt_rest_error_response,
      "! <p class="shorttext synchronized" lang="en">Sets the response HTTP status</p>
      set_status
        IMPORTING
          status        TYPE i DEFAULT cl_rest_status_code=>gc_client_error_bad_request
        RETURNING
          VALUE(result) TYPE REF TO zcl_qdrt_rest_error_response.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_error_msg,
        message   TYPE string,
        callstack TYPE abap_callstack,
      END OF ty_error_msg.

    DATA:
      response TYPE REF TO if_rest_response.
ENDCLASS.



CLASS zcl_qdrt_rest_error_response IMPLEMENTATION.

  METHOD create.
    result = NEW #( ).
    result->response = response.
  ENDMETHOD.


  METHOD set_body_from_text.
    response->create_entity( )->set_string_data(
      zcl_qdrt_json=>serialize(
        data        = VALUE ty_error_msg( message = text )
        pretty_name = zcl_qdrt_json=>pretty_mode-camel_case
        compress    = abap_true ) ).

    result = me.
  ENDMETHOD.


  METHOD set_body_from_exc.
    DATA:
      response_message TYPE ty_error_msg,
      callstack        TYPE abap_callstack.

    response->set_status( error->http_status ).

    response_message-message = error->get_message( ).
    IF response_message-message IS INITIAL.
      response_message-message = 'Internal Server error'.
    ENDIF.

    response_message-callstack = error->callstack.

    response->create_entity( )->set_string_data(
      zcl_qdrt_json=>to_json(
        data        = response_message
        pretty_name = zcl_qdrt_json=>pretty_mode-camel_case
        compress    = abap_true ) ).

    result = me.
  ENDMETHOD.


  METHOD set_status.
    response->set_status( status ).
    result = me.
  ENDMETHOD.


ENDCLASS.

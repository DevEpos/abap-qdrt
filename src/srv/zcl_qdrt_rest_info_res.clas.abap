"! <p class="shorttext synchronized" lang="en">Resource for service Info</p>
CLASS zcl_qdrt_rest_info_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~head
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_rest_info_res IMPLEMENTATION.


  METHOD if_rest_resource~head.
    DATA(csrf_token_header_val) = mo_request->get_header_field( if_rest_request=>gc_header_csrf_token ).
    IF csrf_token_header_val IS INITIAL OR
        to_lower( csrf_token_header_val ) <> 'fetch'.
      mo_response->set_status( 403 ).
      mo_response->set_reason( 'Only fetch for CSRF Allowed' ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.

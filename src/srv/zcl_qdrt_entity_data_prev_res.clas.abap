"! <p class="shorttext synchronized" lang="en">Root Resource for Quick Data Reporting Tool Service</p>
CLASS zcl_qdrt_entity_data_prev_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS if_rest_resource~post
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_entity_data_prev_res IMPLEMENTATION.

  METHOD if_rest_resource~post.
    mo_response->set_status( 200 ).
  ENDMETHOD.

ENDCLASS.

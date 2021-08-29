"! <p class="shorttext synchronized" lang="en">Resource for entity variants</p>
CLASS zcl_qdrt_entity_variant_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get
        REDEFINITION,
      if_rest_resource~post
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_entity_variant_res IMPLEMENTATION.


  METHOD if_rest_resource~get.

  ENDMETHOD.


  METHOD if_rest_resource~post.

  ENDMETHOD.


ENDCLASS.

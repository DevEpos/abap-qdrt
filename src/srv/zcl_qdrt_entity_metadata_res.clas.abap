"! <p class="shorttext synchronized" lang="en">Resource for entity metadata</p>
CLASS zcl_qdrt_entity_metadata_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_entity_metadata_res IMPLEMENTATION.


  METHOD if_rest_resource~get.

  ENDMETHOD.


ENDCLASS.

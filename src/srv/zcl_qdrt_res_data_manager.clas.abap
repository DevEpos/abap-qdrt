"! <p class="shorttext synchronized" lang="en">Root Resource for Quick Data Reporting Tool Service</p>
CLASS zcl_qdrt_res_data_manager DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS if_rest_resource~get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_sub_type,
        fieldname TYPE fieldname,
        tabname   TYPE tabname,
      END OF ty_s_sub_type,
      ty_t_sub_type TYPE STANDARD TABLE OF ty_s_sub_type WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_type,
        id     TYPE string,
        name   TYPE string,
        fields TYPE ty_t_sub_type,
      END OF ty_s_type.
ENDCLASS.



CLASS zcl_qdrt_res_data_manager IMPLEMENTATION.

  METHOD if_rest_resource~get.
    DATA(ls_type_result) = VALUE ty_s_type(
        id     = '1'
        name   = 'Long name'
        fields = VALUE #(
          ( fieldname = 'Product' tabname = 'I_Product' )
        )
    ).
    DATA(lo_response_entity) = mo_response->create_entity( ).

    lo_response_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    DATA(lv_json) = /ui2/cl_json=>serialize(
       data             = ls_type_result
       pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
       name_mappings    = value #(
          ( abap = 'fields'  json = 'Fields' )
          ( abap = 'fieldname' json = 'FieldName' )
          ( abap = 'id' json = 'Id' )
          ( abap = 'name' json = 'Name' )
          ( abap = 'tabname' json = 'TabName' )
       )
    ).
    mo_response->set_status( 200 ).
    lo_response_entity->set_binary_data( /ui2/cl_json=>string_to_raw( lv_json ) ).
  ENDMETHOD.

ENDCLASS.
